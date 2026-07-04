{-# LANGUAGE CPP #-}

-- | The generator-facing engine channel: the per-test-case operations a
-- generator draws from.
--
-- These are plain functions over a 'TestCase', with no @DataSource@ typeclass
-- to implement, since libhegel is the only engine.
--
-- Each primitive has one typed draw, and the engine does no compound
-- generation, so lists, sets, maps, tuples, and choices are composed
-- client-side from spans and collections in "Hegel.Collection" and
-- "Hegel.Gen.Internal".
--
-- String draws go through a caller-owned 'HegelStringGenerator' handle, built
-- once by a @build*Gen@ constructor and drawn from any number of times.
--
-- Pools and state machines also live here.
module Hegel.Internal.DataSource
  ( -- * Generation
    HegelStringGenerator,
    drawBool,
    drawInteger,
    FloatSpec (..),
    drawFloat,
    drawBytes,
    drawUuid,
    drawString,
    TextSpec (..),
    buildTextGen,
    buildRegexGen,
    buildUrlGen,
    buildDomainGen,

    -- * String-generator handle census
    -- $census
    currentLiveStringGenerators,
    settleStringGenerators,

    -- * Errors
    InvariantViolation (..),

    -- * Collections
    newCollection,
    collectionMore,
    collectionReject,

    -- * Pools
    newPool,
    poolAdd,
    poolAddFrom,
    labelPool,
    poolGenerate,

    -- * State machines
    newStateMachine,
    stateMachineNextRule,

    -- * Spans
    Label (..),
    startSpan,
    stopSpan,
  )
where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (void)
import Data.Bits (bit, shiftL, shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef)
#ifdef HEGEL_CENSUS
import Data.IORef (atomicModifyIORef')
#endif
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign (ForeignPtr, Ptr, alloca, allocaBytes, castPtr, nullPtr, peek, withArray, withForeignPtr, withMany)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt, CSize (..))
import Foreign.Concurrent qualified as Concurrent
import Hegel.Internal.Control (AssumeRejected (..), TestStopped (..))
import Hegel.Internal.Event qualified as Event
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.TestCase (Handle (..), TestCase (..), recordDraw)
import Hegel.Internal.Tick qualified as Tick
import System.IO.Unsafe (unsafePerformIO)
import Witch qualified

-- * Generation

-- | Interpret a return code from a per-test-case operation.
--
-- The engine may signal 'HEGEL_E_STOP_TEST' or 'HEGEL_E_ASSUME' as ordinary
-- control flow rather than an error, so map those to the exceptions the
-- generator layer expects.
--
-- Everything else falls through to 'throwOnError'.
handleReturnCode :: TestCase -> CInt -> IO ()
handleReturnCode _ HEGEL_E_STOP_TEST = throwIO TestStopped
handleReturnCode _ HEGEL_E_ASSUME = throwIO AssumeRejected
handleReturnCode tc rc = throwOnError tc.handle.ctx rc

-- | Draw a single boolean that is 'True' with probability @p@ (clamped to
-- @[0,1]@ by the engine).
--
-- Throws 'TestStopped' on exhaustion.
drawBool :: TestCase -> Double -> IO Bool
drawBool tc p =
  withSlotOf tc.slot \outValue -> do
    -- has_forced = 0: forced-draw support is unused (see 'hegel_generate_boolean').
    hegel_generate_boolean tc.handle.ctx tc.handle.ptr (CDouble p) (CBool 0) (CBool 0) outValue
      >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outValue

-- | Draw an integer in the inclusive range @[lo, hi]@, dispatching to the
-- fixed-width @int64_t@ path when both bounds fit, else the
-- arbitrary-precision path. Used both by "Hegel.Gen.Integer" (for 'Word'\/
-- 'Word64', whose full range exceeds 'Int64') and by "Hegel.Gen.Internal"'s
-- @oneOf@\/@frequency@ index draws.
--
-- Throws 'TestStopped' on exhaustion.
drawInteger :: TestCase -> Integer -> Integer -> IO Integer
drawInteger tc lo hi
  | fitsInt64 lo,
    fitsInt64 hi =
      withSlotOf tc.slot \outValue -> do
        hegel_generate_integer tc.handle.ctx tc.handle.ptr (fromInteger lo) (fromInteger hi) outValue
          >>= handleReturnCode tc
        toInteger <$> (peek outValue :: IO Int64)
  | otherwise = drawIntegerBig tc lo hi
  where
    fitsInt64 n = n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Int64)

-- | The 'drawInteger' fallback for bounds outside the @int64_t@ range,
-- reachable via 'Word'\/'Word64' at their default full-type bounds.
drawIntegerBig :: TestCase -> Integer -> Integer -> IO Integer
drawIntegerBig tc lo hi =
  BS.useAsCStringLen (encodeSignedLE lo) \(loPtr, loLen) ->
    BS.useAsCStringLen (encodeSignedLE hi) \(hiPtr, hiLen) -> do
      let cap = max loLen hiLen
      allocaBytes cap \outPtr ->
        alloca \outLenPtr -> do
          hegel_generate_integer_big
            tc.handle.ctx
            tc.handle.ptr
            (castPtr loPtr)
            (fromIntegral loLen)
            (castPtr hiPtr)
            (fromIntegral hiLen)
            (castPtr outPtr)
            (fromIntegral cap)
            outLenPtr
            >>= handleReturnCode tc
          decodeSignedLE <$> BS.packCStringLen (castPtr outPtr, cap)

-- | Minimal number of bytes needed to represent @n@ in two's-complement.
minimalSignedByteLen :: Integer -> Int
minimalSignedByteLen n = go 1
  where
    go k
      | n >= negate (bit (8 * k - 1)) && n < bit (8 * k - 1) = k
      | otherwise = go (k + 1)

-- | Encode a signed 'Integer' as minimal-length two's-complement
-- little-endian bytes — the wire format @hegel_generate_integer_big@ expects
-- for its bounds (mirrors Rust's @BigInt::to_signed_bytes_le@ on the engine
-- side).
encodeSignedLE :: Integer -> ByteString
encodeSignedLE n = BS.pack [byteAt i | i <- [0 .. k - 1]]
  where
    k = minimalSignedByteLen n
    u = if n < 0 then n + bit (8 * k) else n
    byteAt i = fromInteger ((u `shiftR` (8 * i)) .&. 0xff)

-- | Decode a fixed-width two's-complement little-endian buffer — as written
-- by @hegel_generate_integer_big@, sign-extended out to the full requested
-- capacity — back to a signed 'Integer'.
decodeSignedLE :: ByteString -> Integer
decodeSignedLE bs
  | k == 0 = 0
  | testBit u (8 * k - 1) = u - bit (8 * k)
  | otherwise = u
  where
    k = BS.length bs
    u = sum [toInteger (BS.index bs i) `shiftL` (8 * i) | i <- [0 .. k - 1]]

-- | Floating-point draw parameters, mirroring @hegel_generate_float@'s
-- bound\/exclusion\/allow-toggle vocabulary directly.
data FloatSpec = FloatSpec
  { minValue :: !Double,
    maxValue :: !Double,
    allowNan :: !Bool,
    allowInfinity :: !Bool,
    excludeMin :: !Bool,
    excludeMax :: !Bool,
    -- | Nonzero magnitudes below this are never drawn; must be positive and
    -- finite. Pass the width's smallest subnormal for \"no restriction\".
    smallestNonzeroMagnitude :: !Double
  }

-- | Draw a float of the given width (32 or 64), per 'FloatSpec'.
--
-- Throws 'TestStopped' on exhaustion.
drawFloat :: TestCase -> Word32 -> FloatSpec -> IO Double
drawFloat tc width spec =
  withSlotOf tc.slot \outValue -> do
    hegel_generate_float
      tc.handle.ctx
      tc.handle.ptr
      width
      (CDouble spec.minValue)
      (CDouble spec.maxValue)
      (CBool (if spec.allowNan then 1 else 0))
      (CBool (if spec.allowInfinity then 1 else 0))
      (CBool (if spec.excludeMin then 1 else 0))
      (CBool (if spec.excludeMax then 1 else 0))
      (CDouble spec.smallestNonzeroMagnitude)
      outValue
      >>= handleReturnCode tc
    (\(CDouble d) -> d) <$> peek outValue

-- | Draw a byte string with length in the inclusive range @[lo, hi]@.
--
-- Throws 'TestStopped' on exhaustion.
drawBytes :: TestCase -> Word64 -> Word64 -> IO ByteString
drawBytes tc lo hi =
  withSlotOf tc.slot \outResult -> do
    hegel_generate_bytes tc.handle.ctx tc.handle.ptr lo hi outResult >>= handleReturnCode tc
    -- 'finally': a successful draw leaves an engine-allocated buffer that only
    -- the free call releases, and 'BS.packCStringLen' or an async exception
    -- can throw between the draw and the free.
    --
    -- '_result_free' is documented safe on a zeroed struct, so running it
    -- unconditionally cannot double-free.
    let unpack = do
          result <- peek outResult
          BS.packCStringLen (castPtr result.resultData, fromIntegral result.resultLen)
    unpack `finally` void (hegel_generate_bytes_result_free tc.handle.ctx outResult)

-- | Draw a UUID as 16 big-endian bytes. 'Just' pins the RFC 4122 version
-- nibble (and the RFC 4122 variant nibble); 'Nothing' draws uniformly
-- (excluding the nil UUID).
--
-- Throws 'TestStopped' on exhaustion.
drawUuid :: TestCase -> Maybe Word8 -> IO ByteString
drawUuid tc mVersion =
  withSlotBytes 16 tc.slot \outBytes -> do
    hegel_generate_uuid
      tc.handle.ctx
      tc.handle.ptr
      (fromMaybe 0 mVersion)
      (CBool (if isJust mVersion then 1 else 0))
      outBytes
      >>= handleReturnCode tc
    BS.packCStringLen (castPtr outBytes, 16)

-- | Draw a string from a generator built by a @build*Gen@ constructor below.
--
-- Throws 'TestStopped' on exhaustion, 'AssumeRejected' when the draw rejects
-- itself (e.g. an over-length email), and 'InvariantViolation' if the
-- engine's UTF-8 guarantee somehow doesn't hold.
drawString :: TestCase -> ForeignPtr HegelStringGenerator -> IO Text
drawString tc genFP =
  withForeignPtr genFP \genPtr ->
    withSlotOf tc.slot \outResult -> do
      hegel_generate_string tc.handle.ctx tc.handle.ptr genPtr outResult >>= handleReturnCode tc
      -- See 'drawBytes's comment: 'finally' guards the same
      -- draw-then-free-the-engine-buffer window against a throwing
      -- 'BS.packCStringLen' or an async exception.
      let unpack = do
            result <- peek outResult
            BS.packCStringLen (result.resultData, fromIntegral result.resultLen)
      bs <- unpack `finally` void (hegel_generate_string_result_free tc.handle.ctx outResult)
      case TE.decodeUtf8' bs of
        Right t -> pure t
        Left err ->
          throwIO
            InvariantViolation {detail = "libhegel: non-UTF-8 string draw (" <> T.pack (show err) <> ")"}

-- * String-generator construction

-- $census
--
-- A census of live 'HegelStringGenerator' handles, for profiling and
-- testing. This only tracks the Haskell-side 'ForeignPtr' bookkeeping, not
-- the native @hegel_string_generator_t@ allocation. It's a reliable proxy
-- for whether the native handle got freed, since the same finalizer that
-- decrements the count also calls 'hegel_string_generator_free'.

-- | Number of 'HegelStringGenerator' handles currently live, per this
-- process's 'wrapStringGenerator' bookkeeping.
liveStringGenerators :: IORef Int
liveStringGenerators = unsafePerformIO (newIORef 0)
{-# NOINLINE liveStringGenerators #-}

-- | Read the current count without forcing a GC.
currentLiveStringGenerators :: IO Int
currentLiveStringGenerators = readIORef liveStringGenerators

-- | Encourage a settle by generating (and immediately discarding) real
-- allocation pressure, then return the settled count.
--
-- This deliberately doesn't call 'System.Mem.performGC'. In a busy,
-- many-threaded process, GHC's RTS doesn't reliably respawn the finalizer
-- thread that frees these handles, no matter how many explicit major GCs
-- run. Organic allocation pressure reclaims far more reliably, though it's
-- still not a hard guarantee under heavy sustained concurrency. A caller
-- that needs a reliable answer should run in its own quiet process instead.
--
-- The census only tracks handles when built with the @census@ cabal flag.
-- Without it, 'currentLiveStringGenerators' always reads 0 and this is a
-- plain no-op read.
settleStringGenerators :: IO Int
#ifdef HEGEL_CENSUS
settleStringGenerators = do
  mapM_ churnRound [1 .. rounds :: Int]
  currentLiveStringGenerators
  where
    rounds = 1000 :: Int
    chunkSize = 500_000 :: Int
    churnRound r = do
      -- 'BS.replicate' is a real array allocation, an FFI 'memset'. Unlike
      -- @length (replicate n x)@, which GHC's optimizer collapses to a no-op
      -- at @-O1@, this reliably allocates real, short-lived garbage every
      -- round.
      let !bs = BS.replicate chunkSize (fromIntegral r)
      BS.length bs `seq` pure ()
#else
settleStringGenerators = currentLiveStringGenerators
#endif

-- | Wrap a caller-owned @hegel_string_generator_t*@ in a GC-managed
-- 'ForeignPtr' that frees it on finalization.
--
-- The free call takes a @hegel_context_t*@ purely for diagnostics, so the
-- finalizer opens (and closes) its own throwaway context rather than
-- capturing the one that built the generator — the generator's lifetime can
-- outlast that construction context by an arbitrary margin.
wrapStringGenerator :: Ptr HegelStringGenerator -> IO (ForeignPtr HegelStringGenerator)
wrapStringGenerator genPtr = do
  fp <- Concurrent.newForeignPtr genPtr finalizer
  bumpCensus
  pure fp
  where
    finalizer = dropCensus >> withContext \ctx -> void (hegel_string_generator_free ctx genPtr)

#ifdef HEGEL_CENSUS
bumpCensus :: IO ()
bumpCensus = atomicModifyIORef' liveStringGenerators \n -> (n + 1, ())

dropCensus :: IO ()
dropCensus = atomicModifyIORef' liveStringGenerators \n -> (n - 1, ())
#else
bumpCensus :: IO ()
bumpCensus = pure ()

dropCensus :: IO ()
dropCensus = pure ()
#endif

-- | Text-generator construction parameters, mirroring
-- @hegel_string_generator_text@'s vocabulary directly. @categories@\/
-- @excludeCategories@ restrict\/remove Unicode general categories (@Just []@
-- for @categories@ means an empty alphabet, distinct from @Nothing@'s no
-- restriction); @includeCharacters@\/@excludeCharacters@ union\/remove
-- individual characters last.
data TextSpec = TextSpec
  { minSize :: !Word64,
    maxSize :: !Word64,
    -- | Selects the alphabet's base range; 'Nothing' is all Unicode.
    codec :: !(Maybe Text),
    minCodepoint :: !Word32,
    maxCodepoint :: !Word32,
    categories :: !(Maybe [Text]),
    excludeCategories :: !(Maybe [Text]),
    includeCharacters :: !(Maybe Text),
    excludeCharacters :: !(Maybe Text)
  }

-- | Build a __text__ string generator (@hegel_string_generator_text@) per
-- 'TextSpec'.
--
-- Called once per 'Hegel.Gen.Internal.Gen' value (cached by the leaf); throws
-- 'HegelError' on an invalid combination (e.g. an unknown codec\/category).
buildTextGen :: TextSpec -> IO (ForeignPtr HegelStringGenerator)
buildTextGen spec =
  withContext \ctx ->
    withNullableText spec.codec \codecPtr ->
      withNullableTextArray spec.categories \(catsPtr, catsLen) ->
        withNullableTextArray spec.excludeCategories \(exclCatsPtr, exclCatsLen) ->
          withNullableUtf8 spec.includeCharacters \(inclPtr, inclLen) ->
            withNullableUtf8 spec.excludeCharacters \(exclPtr, exclLen) ->
              alloca \outGen -> do
                hegel_string_generator_text
                  ctx
                  spec.minSize
                  spec.maxSize
                  codecPtr
                  spec.minCodepoint
                  spec.maxCodepoint
                  catsPtr
                  catsLen
                  exclCatsPtr
                  exclCatsLen
                  inclPtr
                  inclLen
                  exclPtr
                  exclLen
                  outGen
                  >>= throwOnError ctx
                peek outGen >>= wrapStringGenerator

-- | Build a __regex__ string generator (@hegel_string_generator_regex@).
-- @alphabet@, when given, must itself be a __text__ generator (built by
-- 'buildTextGen') constraining the pattern's padding and wildcard
-- characters.
buildRegexGen :: Text -> Bool -> Maybe (ForeignPtr HegelStringGenerator) -> IO (ForeignPtr HegelStringGenerator)
buildRegexGen pat fullmatch mAlphabet =
  withContext \ctx ->
    CString.withText pat \patPtr ->
      withNullableAlphabet mAlphabet \alphaPtr ->
        alloca \outGen -> do
          hegel_string_generator_regex ctx patPtr (CBool (if fullmatch then 1 else 0)) alphaPtr outGen
            >>= throwOnError ctx
          peek outGen >>= wrapStringGenerator
  where
    withNullableAlphabet :: Maybe (ForeignPtr HegelStringGenerator) -> (Ptr HegelStringGenerator -> IO a) -> IO a
    withNullableAlphabet Nothing k = k nullPtr
    withNullableAlphabet (Just fp) k = withForeignPtr fp k

-- | Build a __URL__ string generator (@hegel_string_generator_url@),
-- producing RFC 3986 @http@\/@https@ URLs.
buildUrlGen :: IO (ForeignPtr HegelStringGenerator)
buildUrlGen =
  withContext \ctx ->
    alloca \outGen -> do
      hegel_string_generator_url ctx outGen >>= throwOnError ctx
      peek outGen >>= wrapStringGenerator

-- | Build a __domain-name__ string generator (@hegel_string_generator_domain@),
-- producing RFC 1035 FQDNs of total length at most @maxLength@ (4..=255).
buildDomainGen :: Word64 -> IO (ForeignPtr HegelStringGenerator)
buildDomainGen maxLen =
  withContext \ctx ->
    alloca \outGen -> do
      hegel_string_generator_domain ctx maxLen outGen >>= throwOnError ctx
      peek outGen >>= wrapStringGenerator

-- | Marshal a nullable 'Text' to a NUL-terminated 'CString', or 'nullPtr'
-- for 'Nothing'.
withNullableText :: Maybe Text -> (CString -> IO a) -> IO a
withNullableText Nothing k = k nullPtr
withNullableText (Just t) k = CString.withText t k

-- | Marshal a nullable list of NUL-terminated UTF-8 names (codec\/category
-- names) to an array pointer and length. 'Nothing' is \"absent\" (@nullPtr@);
-- @Just []@ is \"present and empty\", which libhegel distinguishes (an empty
-- category list means an empty alphabet).
withNullableTextArray :: Maybe [Text] -> ((Ptr CString, CSize) -> IO a) -> IO a
withNullableTextArray Nothing k = k (nullPtr, 0)
withNullableTextArray (Just ts) k =
  withMany CString.withText ts \ptrs ->
    withArray ptrs \arr -> k (arr, fromIntegral (length ts))

-- | Marshal a nullable 'Text' to its raw UTF-8 bytes (pointer + length, not
-- NUL-terminated — these are libhegel's length-delimited character-set
-- arguments, which may legitimately contain U+0000).
withNullableUtf8 :: Maybe Text -> ((Ptr Word8, CSize) -> IO a) -> IO a
withNullableUtf8 Nothing k = k (nullPtr, 0)
withNullableUtf8 (Just t) k =
  BS.useAsCStringLen (TE.encodeUtf8 t) \(p, len) -> k (castPtr p, fromIntegral len)

-- * Errors

-- | An engine guarantee we depend on didn't hold: a string draw wasn't valid
-- UTF-8, or a URL draw wasn't a parseable 'Network.URI.URI'. Both are engine
-- bugs (the engine documents UTF-8 string output and RFC-3986-valid URLs)
-- rather than user-facing errors, hence distinct from 'AssumeRejected'\/
-- 'TestStopped'.
newtype InvariantViolation = InvariantViolation {detail :: Text}
  deriving stock (Show)

instance Exception InvariantViolation

-- * Collections

-- | Begin a variable-length collection; returns its integer ID.
--
-- Throws 'TestStopped' on exhaustion.
newCollection :: TestCase -> Int -> Maybe Int -> IO Int
newCollection tc minSz maxSz =
  withSlotOf tc.slot \outId -> do
    hegel_new_collection tc.handle.ctx tc.handle.ptr (fromIntegral minSz) (maybe maxBound fromIntegral maxSz) outId
      >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- | Ask whether the engine wants another element.
--
-- Throws 'TestStopped' on exhaustion.
collectionMore :: TestCase -> Int -> IO Bool
collectionMore tc cid =
  withSlotOf tc.slot \outMore -> do
    hegel_collection_more tc.handle.ctx tc.handle.ptr (fromIntegral cid) outMore >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outMore

-- | Notify the engine that the last element was rejected.
--
-- Throws 'TestStopped' if the engine gives up.
collectionReject :: TestCase -> Int -> Maybe Text -> IO ()
collectionReject tc cid mWhy =
  case mWhy of
    Nothing -> do
      result <- hegel_collection_reject tc.handle.ctx tc.handle.ptr (fromIntegral cid) nullPtr
      handleReturnCode tc result
    Just why -> CString.withText why \p -> do
      result <- hegel_collection_reject tc.handle.ctx tc.handle.ptr (fromIntegral cid) p
      handleReturnCode tc result

-- * Pools

-- | Create a new variable pool; returns its ID.
--
-- Throws 'TestStopped' on exhaustion.
newPool :: TestCase -> IO Int
newPool tc =
  withSlotOf tc.slot \outId -> do
    hegel_new_pool tc.handle.ctx tc.handle.ptr outId >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)

-- | Register a new variable in the pool; returns the engine-assigned
-- variable id.
--
-- Records a 'Event.Born' event (this, 'poolAddFrom', 'poolGenerate', and
-- 'labelPool' are the only pool emission points, so 'Hegel.Pool' needs no
-- event awareness).
poolAdd :: TestCase -> Int -> IO Int
poolAdd tc pid = poolAddWith tc pid Nothing

-- | 'poolAdd' with a declared lineage: the new variable continues the given
-- source var's logical value (the destination half of 'Hegel.Pool.transfer').
poolAddFrom :: TestCase -> Int -> Event.Var -> IO Int
poolAddFrom tc pid from = poolAddWith tc pid (Just from)

poolAddWith :: TestCase -> Int -> Maybe Event.Var -> IO Int
poolAddWith tc pid lineage = do
  vid <- withSlotOf tc.slot \outId -> do
    hegel_pool_add tc.handle.ctx tc.handle.ptr (fromIntegral pid) outId >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)
  Tick.record tc.recording tc.events \c ->
    Event.Event {clock = c, var = Event.Var {pool = pid, id = vid}, kind = Event.Born lineage}
  pure vid

-- | Record a pool's display label ('Hegel.Pool.named'). No engine call —
-- labels are report vocabulary; the event stream is their only channel to
-- the renderer.
labelPool :: TestCase -> Int -> Text -> IO ()
labelPool tc pid label =
  Tick.record tc.recording tc.events \c ->
    Event.Event {clock = c, var = Event.Var {pool = pid, id = 0}, kind = Event.Named label}

-- | Draw a variable id from the pool.
--
-- Pass 'True' to consume the variable (remove it from the pool). Records a
-- 'Event.Reused'\/'Event.Consumed' event; a consuming draw is the value's
-- death (the engine has no @pool_remove@).
--
-- Throws 'AssumeRejected' when the pool is empty, discarding the test case.
poolGenerate :: TestCase -> Int -> Bool -> IO Int
poolGenerate tc pid consume = do
  vid <- withSlotOf tc.slot \outId -> do
    hegel_pool_generate tc.handle.ctx tc.handle.ptr (fromIntegral pid) (CBool (if consume then 1 else 0)) outId
      >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)
  let var = Event.Var {pool = pid, id = vid}
  Tick.record tc.recording tc.events \c ->
    Event.Event
      { clock = c,
        var,
        kind = if consume then Event.Consumed else Event.Reused
      }
  -- Tag this draw so the enclosing 'forAll' can bind its rendered value to
  -- this pool 'Var' (see Note [Draw provenance].
  recordDraw tc var
  pure vid

-- * State machines

-- | Register an engine-owned state machine; returns its ID.
--
-- @ruleNames@ must be non-empty.
--
-- Throws 'TestStopped' on exhaustion.
newStateMachine :: TestCase -> [Text] -> [Text] -> IO Int
newStateMachine tc ruleNames invariantNames =
  withMany CString.withText ruleNames \rulePtrs ->
    withMany CString.withText invariantNames \invPtrs ->
      withArray rulePtrs \rulesArr ->
        withArray invPtrs \invArr ->
          withSlotOf tc.slot \outId -> do
            hegel_new_state_machine
              tc.handle.ctx
              tc.handle.ptr
              rulesArr
              (fromIntegral (length ruleNames))
              invArr
              (fromIntegral (length invariantNames))
              outId
              >>= handleReturnCode tc
            fromIntegral <$> (peek outId :: IO Int64)

-- | Draw the next rule index for the state machine.
--
-- Throws 'TestStopped' when the choice budget is exhausted.
stateMachineNextRule :: TestCase -> Int -> IO Int
stateMachineNextRule tc mid =
  withSlotOf tc.slot \outIdx -> do
    hegel_state_machine_next_rule tc.handle.ctx tc.handle.ptr (fromIntegral mid) outIdx
      >>= handleReturnCode tc
    fromIntegral <$> (peek outIdx :: IO Int64)

-- * Spans

-- | Open a labeled span.
startSpan :: TestCase -> Label -> IO ()
startSpan tc label = do
  result <- hegel_start_span tc.handle.ctx tc.handle.ptr (Witch.into @Word64 label)
  throwOnError tc.handle.ctx result

-- | Close the most-recently-opened span.
-- Pass 'True' to mark it discarded.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard = do
  result <- hegel_stop_span tc.handle.ctx tc.handle.ptr (CBool (if isDiscard then 1 else 0))
  throwOnError tc.handle.ctx result

-- | Span labels used to group related draws so the engine can shrink them
-- as a unit. Numeric values match @libhegel@'s constants.
data Label
  = LabelList
  | LabelListElement
  | LabelSet
  | LabelSetElement
  | LabelMap
  | LabelMapEntry
  | LabelTuple
  | LabelOneOf
  | LabelOptional
  | LabelFixedDict
  | LabelFlatMap
  | LabelFilter
  | LabelMapped
  | LabelSampledFrom
  | LabelEnumVariant
  | LabelFeatureFlag
  deriving stock (Show)

-- | The @hegel_label_t@ wire identifier (the @HEGEL_LABEL_*@ constants are the
-- single source of truth).
instance Witch.From Label Word64 where
  from LabelList = HEGEL_LABEL_LIST
  from LabelListElement = HEGEL_LABEL_LIST_ELEMENT
  from LabelSet = HEGEL_LABEL_SET
  from LabelSetElement = HEGEL_LABEL_SET_ELEMENT
  from LabelMap = HEGEL_LABEL_MAP
  from LabelMapEntry = HEGEL_LABEL_MAP_ENTRY
  from LabelTuple = HEGEL_LABEL_TUPLE
  from LabelOneOf = HEGEL_LABEL_ONE_OF
  from LabelOptional = HEGEL_LABEL_OPTIONAL
  from LabelFixedDict = HEGEL_LABEL_FIXED_DICT
  from LabelFlatMap = HEGEL_LABEL_FLAT_MAP
  from LabelFilter = HEGEL_LABEL_FILTER
  from LabelMapped = HEGEL_LABEL_MAPPED
  from LabelSampledFrom = HEGEL_LABEL_SAMPLED_FROM
  from LabelEnumVariant = HEGEL_LABEL_ENUM_VARIANT
  from LabelFeatureFlag = HEGEL_LABEL_FEATURE_FLAG

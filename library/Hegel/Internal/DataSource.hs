{-# LANGUAGE CPP #-}

module Hegel.Internal.DataSource
  ( -- * Generation
    HegelStringGenerator,
    HegelCollection,
    HegelPool,
    HegelStateMachine,
    HegelRecursion,
    drawBool,
    drawInteger,
    FloatSpec (..),
    drawFloat,
    drawBytes,
    drawUuid,
    drawDate,
    drawTime,
    drawDatetime,
    drawString,
    TextSpec (..),
    buildTextGen,
    buildRegexGen,
    buildEmailGen,
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
    freeCollection,

    -- * Pools
    newPool,
    poolAdd,
    poolAddFrom,
    labelPool,
    poolGenerate,
    freePool,
    freshPoolIdentity,

    -- * State machines
    newStateMachine,
    newConcurrentStateMachine,
    stateMachineNextGroup,
    stateMachineNextRule,
    stateMachineRuleRejected,
    freeStateMachine,

    -- * Recursive generation
    newRecursion,
    recursionBranch,
    recursionLeaf,
    recursionRetry,
    recursionFinish,
    freeRecursion,

    -- * Spans
    Label (..),
    startSpan,
    stopSpan,
  )
where

import Control.Exception (finally, throwIO)
import Control.Monad (void)
import Data.Bits (bit, shiftL, shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fixed (Fixed (MkFixed), Pico)
import GHC.Stack (HasCallStack)
import Hegel.Exception (InvariantViolation (..))
#ifdef HEGEL_CENSUS
import Data.Foldable (traverse_)
#endif
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Calendar (Day, fromGregorianValid, toGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), makeTimeOfDayValid)
import Data.Word (Word32, Word64, Word8)
import Foreign (ForeignPtr, Ptr, alloca, allocaBytes, castPtr, nullPtr, peek, with, withArray, withForeignPtr, withMany)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CInt, CSize (..))
import Foreign.Concurrent qualified as Concurrent
import Hegel.Internal.Control (AssumeRejected (..), AttemptMispriced (..), LeafBudgetExceeded (..), TestStopped (..), malformedTest)
import Hegel.Internal.Event qualified as Event
import Hegel.Internal.Foreign.CString qualified as CString
import Hegel.Internal.Foreign.Raw
import Hegel.Internal.TestCase (Handle (..), TestCase (..), recordDraw)
import Hegel.Internal.Tick qualified as Tick
import System.IO.Unsafe (unsafePerformIO)
import Witch qualified

-- * Generation

-- | Interpret a test case operation's return code.
handleReturnCode :: TestCase -> CInt -> IO ()
handleReturnCode _ HEGEL_E_STOP_TEST = throwIO TestStopped
handleReturnCode _ HEGEL_E_ASSUME = throwIO AssumeRejected
handleReturnCode tc rc = throwOnError tc.handle.ctx rc

-- | Draw a single boolean that is 'True' with probability @p@.
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
-- fixed-width @int64_t@ path when both bounds fit.
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

-- | The 'drawInteger' fallback for bounds outside the @int64_t@.
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

-- | Encode a signed 'Integer' as minimal-length two's-complement little-endian;
-- this is the format that @hegel_generate_integer_big@ expects for its bounds.
encodeSignedLE :: Integer -> ByteString
encodeSignedLE n = BS.pack [byteAt i | i <- [0 .. k - 1]]
  where
    k = minimalSignedByteLen n
    u = if n < 0 then n + bit (8 * k) else n
    byteAt i = fromInteger ((u `shiftR` (8 * i)) .&. 0xff)

-- | Decode a fixed-width two's-complement little-endian buffer, as written
-- by @hegel_generate_integer_big@, back to an 'Integer'.
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
-- nibble (and the RFC 4122 variant nibble); 'Nothing' draws uniformly.
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

-- | Draw a date in the inclusive range @[lo, hi]@, shrinking toward
-- 2000-01-01 or the nearest bound in range.
--
-- Throws 'TestStopped' on exhaustion.
drawDate :: TestCase -> Day -> Day -> IO Day
drawDate tc lo hi =
  with (dayToHegelDate lo) \loPtr ->
    with (dayToHegelDate hi) \hiPtr ->
      alloca \outPtr -> do
        hegel_generate_date tc.handle.ctx tc.handle.ptr loPtr hiPtr outPtr >>= handleReturnCode tc
        out <- peek outPtr
        case hegelDateToDay out of
          Just d -> pure d
          Nothing ->
            throwIO InvariantViolation {detail = "libhegel: date draw returned an invalid calendar date"}

-- | Draw a time of day in the inclusive range @[lo, hi]@, shrinking toward
-- @lo@.
--
-- Throws 'TestStopped' on exhaustion.
drawTime :: TestCase -> TimeOfDay -> TimeOfDay -> IO TimeOfDay
drawTime tc lo hi =
  with (timeOfDayToHegelTime lo) \loPtr ->
    with (timeOfDayToHegelTime hi) \hiPtr ->
      alloca \outPtr -> do
        hegel_generate_time tc.handle.ctx tc.handle.ptr loPtr hiPtr outPtr >>= handleReturnCode tc
        out <- peek outPtr
        case hegelTimeToTimeOfDay out of
          Just t -> pure t
          Nothing ->
            throwIO InvariantViolation {detail = "libhegel: time draw returned an invalid time of day"}

-- | Draw a naive datetime, no timezone, in the inclusive range @[lo, hi]@,
-- shrinking toward 2000-01-01T00:00:00 clamped into range.
--
-- Throws 'TestStopped' on exhaustion.
drawDatetime :: TestCase -> LocalTime -> LocalTime -> IO LocalTime
drawDatetime tc lo hi =
  with (localTimeToHegelDatetime lo) \loPtr ->
    with (localTimeToHegelDatetime hi) \hiPtr ->
      alloca \outPtr -> do
        hegel_generate_datetime tc.handle.ctx tc.handle.ptr loPtr hiPtr outPtr >>= handleReturnCode tc
        out <- peek outPtr
        case hegelDatetimeToLocalTime out of
          Just lt -> pure lt
          Nothing ->
            throwIO InvariantViolation {detail = "libhegel: datetime draw returned an invalid date or time"}

dayToHegelDate :: Day -> HegelDate
dayToHegelDate d = HegelDate {year = fromIntegral y, month = fromIntegral m, day = fromIntegral dd}
  where
    (y, m, dd) = toGregorian d

hegelDateToDay :: HegelDate -> Maybe Day
hegelDateToDay hd = fromGregorianValid (fromIntegral hd.year) (fromIntegral hd.month) (fromIntegral hd.day)

timeOfDayToHegelTime :: TimeOfDay -> HegelTime
timeOfDayToHegelTime t =
  HegelTime {hour = fromIntegral t.todHour, minute = fromIntegral t.todMin, second = wholeSeconds, microsecond = micros}
  where
    (wholeSeconds, micros) = picoToMicros t.todSec

hegelTimeToTimeOfDay :: HegelTime -> Maybe TimeOfDay
hegelTimeToTimeOfDay ht =
  makeTimeOfDayValid (fromIntegral ht.hour) (fromIntegral ht.minute) (microsToPico ht.second ht.microsecond)

localTimeToHegelDatetime :: LocalTime -> HegelDatetime
localTimeToHegelDatetime lt =
  HegelDatetime {date = dayToHegelDate lt.localDay, time = timeOfDayToHegelTime lt.localTimeOfDay}

hegelDatetimeToLocalTime :: HegelDatetime -> Maybe LocalTime
hegelDatetimeToLocalTime hdt = LocalTime <$> hegelDateToDay hdt.date <*> hegelTimeToTimeOfDay hdt.time

-- | Split a time-of-day second component into whole seconds and a
-- microsecond count. Callers must ensure @s@ carries no finer-than-microsecond
-- precision; 'Hegel.Gen.Time.checkFields' rejects such a bound before this
-- ever runs.
picoToMicros :: Pico -> (Word8, Word32)
picoToMicros (MkFixed ps) = (fromInteger wholeSeconds, fromInteger micros)
  where
    (wholeSeconds, remainder) = ps `divMod` 1_000_000_000_000
    micros = remainder `div` 1_000_000

-- | Combine a whole second count and a microsecond count into a
-- picosecond-precision time-of-day second component.
microsToPico :: Word8 -> Word32 -> Pico
microsToPico wholeSeconds micros =
  MkFixed (toInteger wholeSeconds * 1_000_000_000_000 + toInteger micros * 1_000_000)

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
-- testing. It's a reliable proxy for whether the native handle got freed,
-- since the same finalizer that decrements the count also calls 'hegel_string_generator_free'.

-- | Number of 'HegelStringGenerator' handles currently live, per this
-- process's 'wrapStringGenerator' bookkeeping.
liveStringGenerators :: IORef Int
liveStringGenerators = unsafePerformIO (newIORef 0)
{-# NOINLINE liveStringGenerators #-}

-- | Read the current count without forcing a GC.
currentLiveStringGenerators :: IO Int
currentLiveStringGenerators = readIORef liveStringGenerators

-- | Encourage the RTS to settle by generating and immediately discarding real
-- allocation pressure, then return the settled count.
--
-- The census only tracks handles when built with the @census@ cabal flag.
-- Without it, 'currentLiveStringGenerators' always reads 0 and this is a
-- plain no-op read.
settleStringGenerators :: IO Int
#ifdef HEGEL_CENSUS
settleStringGenerators = do
  traverse_ churnRound [1 .. rounds :: Int]
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

data TextSpec = TextSpec
  { minSize :: !Word64,
    maxSize :: !Word64,
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

-- | Build a __regex__ string generator.
--
-- @alphabet@, when given, must itself be a __text__ generator with constraints
-- on the pattern's padding and wildcard characters.
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

-- | Build an __email-address__ string generator, which produces RFC 5321\/5322
-- addresses such as @alice\@example.com@.
buildEmailGen :: IO (ForeignPtr HegelStringGenerator)
buildEmailGen =
  withContext \ctx ->
    alloca \outGen -> do
      hegel_string_generator_email ctx outGen >>= throwOnError ctx
      peek outGen >>= wrapStringGenerator

-- | Build a __URL__ string generator, which produces RFC 3986 @http@\/@https@
-- URLs.
buildUrlGen :: IO (ForeignPtr HegelStringGenerator)
buildUrlGen =
  withContext \ctx ->
    alloca \outGen -> do
      hegel_string_generator_url ctx outGen >>= throwOnError ctx
      peek outGen >>= wrapStringGenerator

-- | Build a __domain-name__ string generator, which produces RFC 1035 FQDNs.
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

-- | Marshal a nullable list of NUL-terminated UTF-8 names to an array pointer
-- & length.
withNullableTextArray :: Maybe [Text] -> ((Ptr CString, CSize) -> IO a) -> IO a
withNullableTextArray Nothing k = k (nullPtr, 0)
withNullableTextArray (Just ts) k =
  withMany CString.withText ts \ptrs ->
    withArray ptrs \arr -> k (arr, fromIntegral (length ts))

-- | Marshal a nullable 'Text' to its raw UTF-8 bytes.
withNullableUtf8 :: Maybe Text -> ((Ptr Word8, CSize) -> IO a) -> IO a
withNullableUtf8 Nothing k = k (nullPtr, 0)
withNullableUtf8 (Just t) k =
  BS.useAsCStringLen (TE.encodeUtf8 t) \(p, len) -> k (castPtr p, fromIntegral len)

-- * Collections

-- | Begin a variable-length collection.
--
-- Throws 'TestStopped' on exhaustion.
newCollection :: TestCase -> Int -> Maybe Int -> IO (Ptr HegelCollection)
newCollection tc minSz maxSz =
  withSlotOf tc.slot \outColl -> do
    hegel_new_collection tc.handle.ctx tc.handle.ptr (fromIntegral minSz) (maybe maxBound fromIntegral maxSz) outColl
      >>= handleReturnCode tc
    peek outColl

-- | Ask whether the engine wants another element.
--
-- Throws 'TestStopped' on exhaustion.
collectionMore :: TestCase -> Ptr HegelCollection -> IO Bool
collectionMore tc coll =
  withSlotOf tc.slot \outMore -> do
    hegel_collection_more tc.handle.ctx tc.handle.ptr coll outMore >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outMore

-- | Notify the engine that the last element was rejected.
--
-- Throws 'TestStopped' if the engine gives up.
collectionReject :: TestCase -> Ptr HegelCollection -> Maybe Text -> IO ()
collectionReject tc coll mWhy =
  case mWhy of
    Nothing -> do
      result <- hegel_collection_reject tc.handle.ctx tc.handle.ptr coll nullPtr
      handleReturnCode tc result
    Just why -> CString.withText why \p -> do
      result <- hegel_collection_reject tc.handle.ctx tc.handle.ptr coll p
      handleReturnCode tc result

-- | Release a collection handle from 'newCollection'; each handle must be
-- freed /exactly/ once.
freeCollection :: TestCase -> Ptr HegelCollection -> IO ()
freeCollection tc coll = void (hegel_collection_free tc.handle.ctx coll)

-- * Pools

-- | Source of the small integer identities 'freshPoolIdentity' hands out.
poolIdentitySource :: IORef Int
poolIdentitySource = unsafePerformIO (newIORef 0)
{-# NOINLINE poolIdentitySource #-}

-- | A fresh integer identity for 'Event.Var' report grouping, unique for
-- the life of the process.
--
-- Call once per pool, at creation, and thread the result through every call
-- that reports an event against that pool.
freshPoolIdentity :: IO Int
freshPoolIdentity = atomicModifyIORef' poolIdentitySource \n -> (n + 1, n)

-- | Create a new variable pool; returns its caller-owned handle.
--
-- Throws 'TestStopped' on exhaustion.
newPool :: TestCase -> IO (Ptr HegelPool)
newPool tc =
  withSlotOf tc.slot \outPool -> do
    hegel_new_pool tc.handle.ctx tc.handle.ptr outPool >>= handleReturnCode tc
    peek outPool

-- | Register a new variable in the pool, returning the engine-assigned
-- variable id; @identity@ is the pool's own 'freshPoolIdentity', for
-- 'Event.Var' report grouping.
poolAdd :: TestCase -> Ptr HegelPool -> Int -> IO Int
poolAdd tc pool identity = poolAddWith tc pool identity Nothing

-- | 'poolAdd' with a declared lineage: the new variable continues the given
-- source var's logical value.
poolAddFrom :: TestCase -> Ptr HegelPool -> Int -> Event.Var -> IO Int
poolAddFrom tc pool identity from = poolAddWith tc pool identity (Just from)

poolAddWith :: TestCase -> Ptr HegelPool -> Int -> Maybe Event.Var -> IO Int
poolAddWith tc pool identity lineage = do
  vid <- withSlotOf tc.slot \outId -> do
    hegel_pool_add tc.handle.ctx tc.handle.ptr pool outId >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)
  Tick.record tc.recording tc.events \c ->
    Event.Event {clock = c, var = Event.Var {pool = identity, id = vid}, kind = Event.Born lineage}
  pure vid

-- | Record a pool's display label.
labelPool :: TestCase -> Int -> Text -> IO ()
labelPool tc identity label =
  Tick.record tc.recording tc.events \c ->
    Event.Event {clock = c, var = Event.Var {pool = identity, id = 0}, kind = Event.Named label}

-- | Draw a variable id from the pool.
--
-- Pass 'True' to consume the variable (remove it from the pool).
--
-- Throws 'AssumeRejected' when the pool is empty, discarding the test case.
poolGenerate :: TestCase -> Ptr HegelPool -> Int -> Bool -> IO Int
poolGenerate tc pool identity consume = do
  vid <- withSlotOf tc.slot \outId -> do
    hegel_pool_generate tc.handle.ctx tc.handle.ptr pool (CBool (if consume then 1 else 0)) outId
      >>= handleReturnCode tc
    fromIntegral <$> (peek outId :: IO Int64)
  let var = Event.Var {pool = identity, id = vid}
  Tick.record tc.recording tc.events \c ->
    Event.Event
      { clock = c,
        var,
        kind = if consume then Event.Consumed else Event.Reused
      }
  -- Tag this draw so the enclosing 'forAll' can bind its rendered value to
  -- this pool 'Var' (see Note [Draw provenance]).
  recordDraw tc var
  pure vid

-- | Release a pool handle from 'newPool'. Each handle must be freed exactly
-- once.
freePool :: TestCase -> Ptr HegelPool -> IO ()
freePool tc pool = void (hegel_pool_free tc.handle.ctx pool)

-- * State machines

-- | Register sequential state machine; returns its handle.
--
-- @ruleNames@ must be non-empty.
newStateMachine :: (HasCallStack) => TestCase -> [Text] -> [Text] -> IO (Ptr HegelStateMachine)
newStateMachine tc ruleNames invariantNames =
  -- One sequential group (id 0) for every rule.
  fst <$> newConcurrentStateMachine tc ruleNames (replicate (length ruleNames) 0) invariantNames 1 1

-- | A generalization of 'newStateMachine' that supports the @libhegel@ round
-- protocol.
--
-- @ruleGroups@ must hold exactly one concurrency-group ID per @ruleNames@
-- entry, in the same order; @ruleNames@ must be non-empty.
newConcurrentStateMachine :: (HasCallStack) => TestCase -> [Text] -> [Int64] -> [Text] -> Int64 -> Int64 -> IO (Ptr HegelStateMachine, Int)
newConcurrentStateMachine tc ruleNames ruleGroups invariantNames minConcurrency maxConcurrency
  | length ruleGroups /= length ruleNames =
      throwIO
        ( malformedTest
            "Hegel.Internal.DataSource.newConcurrentStateMachine"
            "every rule name needs exactly one corresponding rule group"
            [("rules", T.pack (show (length ruleNames))), ("ruleGroups", T.pack (show (length ruleGroups)))]
        )
  | otherwise =
      withMany CString.withText ruleNames \rulePtrs ->
        withMany CString.withText invariantNames \invPtrs ->
          withArray rulePtrs \rulesArr ->
            withArray invPtrs \invArr ->
              withArray ruleGroups \groupsArr ->
                withSlotOf tc.slot \outHandle ->
                  alloca \outConcurrency -> do
                    hegel_new_state_machine
                      tc.handle.ctx
                      tc.handle.ptr
                      rulesArr
                      groupsArr
                      (fromIntegral (length ruleNames))
                      invArr
                      (fromIntegral (length invariantNames))
                      minConcurrency
                      maxConcurrency
                      outHandle
                      outConcurrency
                      >>= handleReturnCode tc
                    handle <- peek outHandle
                    concurrency <- fromIntegral <$> (peek outConcurrency :: IO Int64)
                    pure (handle, concurrency)

-- | Start the machine's next round, or 'Nothing' once the engine has
-- decided the whole state machine is done stepping.
--
-- Call on the root test-case handle at every join point, including before
-- the first rule is requested, even for a sequential machine.
--
-- Throws 'TestStopped' when the choice budget is exhausted, distinct from a
-- clean 'Nothing'.
stateMachineNextGroup :: TestCase -> Ptr HegelStateMachine -> IO (Maybe Int)
stateMachineNextGroup tc sm =
  withSlotOf tc.slot \outGroup -> do
    hegel_state_machine_next_group tc.handle.ctx tc.handle.ptr sm outGroup >>= handleReturnCode tc
    raw <- peek outGroup :: IO Int64
    pure $ if raw == HEGEL_STATE_MACHINE_DONE then Nothing else Just (fromIntegral raw)

-- | Draw the next rule index for worker @workerIndex@ to run this round, or
-- 'Nothing' once its round budget is exhausted.
--
-- Pass @workerIndex = 0@ for a sequential machine.
--
-- Throws 'TestStopped' when the choice budget is exhausted, distinct from a
-- clean 'Nothing'.
stateMachineNextRule :: TestCase -> Ptr HegelStateMachine -> Int -> IO (Maybe Int)
stateMachineNextRule tc sm workerIndex =
  withSlotOf tc.slot \outIdx -> do
    hegel_state_machine_next_rule tc.handle.ctx tc.handle.ptr sm (fromIntegral workerIndex) outIdx
      >>= handleReturnCode tc
    raw <- peek outIdx :: IO Int64
    pure $ if raw == HEGEL_STATE_MACHINE_DONE then Nothing else Just (fromIntegral raw)

-- | Report that the rule most recently handed to worker @workerIndex@ was
-- rejected, so it does not count toward the engine's step budget.
stateMachineRuleRejected :: TestCase -> Ptr HegelStateMachine -> Int -> IO ()
stateMachineRuleRejected tc sm workerIndex = do
  result <- hegel_state_machine_rule_rejected tc.handle.ctx tc.handle.ptr sm (fromIntegral workerIndex)
  handleReturnCode tc result

-- | Release a state-machine handle from 'newStateMachine'. Each handle must
-- be freed exactly once.
freeStateMachine :: TestCase -> Ptr HegelStateMachine -> IO ()
freeStateMachine tc sm = void (hegel_state_machine_free tc.handle.ctx sm)

-- * Recursive generation

-- | Open a recursive generation scope for one recursively defined value;
-- returns its caller-owned handle.
--
-- Throws 'TestStopped' on exhaustion.
newRecursion :: TestCase -> Word64 -> Word64 -> IO (Ptr HegelRecursion)
newRecursion tc maxDep maxLeavesN =
  withSlotOf tc.slot \outRecursion -> do
    hegel_new_recursion tc.handle.ctx tc.handle.ptr maxDep maxLeavesN outRecursion >>= handleReturnCode tc
    peek outRecursion

-- | Draw the leaf-or-branch decision for the sub-value at @depth@: 'True'
-- means invoke the branch function, drawing its own sub-values at
-- @depth + 1@ by this same protocol; 'False' means the sub-value is a leaf,
-- so call 'recursionLeaf' and then draw it.
--
-- Throws 'TestStopped' on exhaustion.
recursionBranch :: TestCase -> Ptr HegelRecursion -> Word64 -> IO Bool
recursionBranch tc recursion depth =
  withSlotOf tc.slot \outBranch -> do
    hegel_recursion_branch tc.handle.ctx tc.handle.ptr recursion depth outBranch >>= handleReturnCode tc
    (/= 0) . (\(CBool b) -> b) <$> peek outBranch

-- | Count one leaf against the current attempt's budget; call immediately
-- before drawing each leaf value.
--
-- Throws 'LeafBudgetExceeded' when the attempt has outgrown its leaf budget;
-- callers then must unwind without drawing anything further and start again
-- from the root.
--
-- Throws 'TestStopped' on exhaustion.
recursionLeaf :: TestCase -> Ptr HegelRecursion -> IO ()
recursionLeaf tc recursion = do
  rc <- hegel_recursion_leaf tc.handle.ctx tc.handle.ptr recursion
  case rc of
    HEGEL_E_RETRY -> throwIO LeafBudgetExceeded
    _ -> handleReturnCode tc rc

-- | Discard a generation attempt after a 'LeafBudgetExceeded'.
recursionRetry :: TestCase -> Ptr HegelRecursion -> IO ()
recursionRetry tc recursion = hegel_recursion_retry tc.handle.ctx tc.handle.ptr recursion >>= handleReturnCode tc

-- | Report that the recursive value's root sub-value has finished generating.
--
-- Throws 'AttemptMispriced' when the completed value was priced for a
-- different branch arity than the branch function actually drew. The engine
-- has already discarded it, so the caller must drop the value and start
-- again from the root, without calling 'recursionRetry'.
--
-- Throws 'TestStopped' on exhaustion.
recursionFinish :: TestCase -> Ptr HegelRecursion -> IO ()
recursionFinish tc recursion = do
  rc <- hegel_recursion_finish tc.handle.ctx tc.handle.ptr recursion
  case rc of
    HEGEL_E_RETRY -> throwIO AttemptMispriced
    _ -> handleReturnCode tc rc

-- | Release a recursion handle from 'newRecursion'.
--
-- __NOTE__: Each handle must be freed exactly once.
freeRecursion :: TestCase -> Ptr HegelRecursion -> IO ()
freeRecursion tc recursion = void (hegel_recursion_free tc.handle.ctx recursion)

-- * Spans

-- | Open a labeled span.
startSpan :: TestCase -> Label -> IO ()
startSpan tc label = do
  result <- hegel_start_span tc.handle.ctx tc.handle.ptr (Witch.into @Word64 label)
  handleReturnCode tc result

-- | Close the most-recently-opened span.
-- Pass 'True' to mark it discarded.
stopSpan :: TestCase -> Bool -> IO ()
stopSpan tc isDiscard = do
  result <- hegel_stop_span tc.handle.ctx tc.handle.ptr (CBool (if isDiscard then 1 else 0))
  handleReturnCode tc result

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
  | LabelStatefulRule
  | LabelRecursive
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
  from LabelStatefulRule = HEGEL_LABEL_STATEFUL_RULE
  from LabelRecursive = HEGEL_LABEL_RECURSIVE

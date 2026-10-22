-- | A loaded Lark grammar, and the engine protocol that gathers its files and
-- compiles it for drawing.
module Hegel.Internal.Grammar
  ( Source (..),
    GatherFailure (..),
    gather,
    compile,
    fingerprintOf,
  )
where

import Control.Exception (bracket, mask_, throwIO)
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import Foreign (ForeignPtr, Ptr, alloca, nullPtr, peek, withForeignPtr)
import Foreign.C.Types (CInt, CSize)
import Foreign.Concurrent qualified as Concurrent
import Hegel.Exception (InvariantViolation (..))
import Hegel.Internal.DataSource (combineLabels, followHostUnicode, labelFromName)
import Hegel.Internal.Foreign.Raw
  ( HegelContext,
    HegelGrammar,
    HegelGrammarBuilder,
    HegelStringGenerator,
    hegel_grammar_builder_file,
    hegel_grammar_builder_free,
    hegel_grammar_builder_new,
    hegel_grammar_builder_provide,
    hegel_grammar_free,
    hegel_grammar_new,
    lastErrorMessage,
    throwOnError,
    withContext,
    pattern HEGEL_E_INVALID_ARG,
    pattern HEGEL_E_NOT_FOUND,
  )
import Hegel.Internal.Foreign.String qualified as String

-- | A grammar's root text and the text of every file its imports reached.
data Source = Source
  { -- | Where the root was read from, for diagnostics.
    root :: !FilePath,
    rootText :: !Text,
    -- | Each imported path as the engine named it, with its text, or
    -- 'Nothing' when no such file exists and a bundled grammar stands in.
    imports :: !(Map FilePath (Maybe Text)),
    -- | A label derived from the content alone, so the same grammar loaded
    -- from two paths gets the same one.
    fingerprint :: !Word64
  }
  deriving stock (Show, Eq)

-- | The engine's rejection of a grammar file while gathering, naming the file,
-- or 'Nothing' for the root text.
data GatherFailure = GatherFailure
  { file :: !(Maybe FilePath),
    message :: !Text
  }
  deriving stock (Show)

-- | Gather the grammar rooted at @rootText@ into a fresh builder, serving each
-- file it imports through @serve@, and run the action on the complete builder.
--
-- @serve@ returns 'Nothing' for a file that doesn't exist. The builder is
-- freed when the action returns, so it must not escape.
gather ::
  Ptr HegelContext ->
  Text ->
  (FilePath -> IO (Maybe Text)) ->
  (Ptr HegelGrammarBuilder -> IO r) ->
  IO (Either GatherFailure r)
gather ctx rootText serve action =
  bracket acquire release \case
    Left failure -> pure (Left failure)
    Right builder -> loop builder 0 >>= traverse (\() -> action builder)
  where
    acquire = alloca \out -> do
      rc <- String.withText rootText \txt -> hegel_grammar_builder_new ctx txt out
      rejectedOr Nothing rc (peek out)

    release = either (const (pure ())) (void . hegel_grammar_builder_free ctx)

    loop :: Ptr HegelGrammarBuilder -> CSize -> IO (Either GatherFailure ())
    loop builder i =
      nextFile builder i >>= \case
        Nothing -> pure (Right ())
        Just path -> do
          contents <- serve path
          rc <- String.withOptText contents (hegel_grammar_builder_provide ctx builder i)
          rejectedOr (Just path) rc (pure ()) >>= \case
            Left failure -> pure (Left failure)
            Right () -> loop builder (i + 1)

    nextFile builder i = alloca \outPath -> do
      rc <- hegel_grammar_builder_file ctx builder i outPath
      if rc == HEGEL_E_NOT_FOUND
        then pure Nothing
        else throwOnError ctx rc *> (Just . T.unpack <$> String.peekText outPath)

    rejectedOr :: Maybe FilePath -> CInt -> IO a -> IO (Either GatherFailure a)
    rejectedOr path rc ok
      | rc == HEGEL_E_INVALID_ARG = do
          msg <- lastErrorMessage ctx
          pure (Left GatherFailure {file = path, message = fromMaybe "the engine gave no reason" msg})
      | otherwise = throwOnError ctx rc *> (Right <$> ok)

-- | Compile a grammar for drawing from @start@, or from Lark's default
-- @start@ rule.
--
-- A zero @maxSize@ or @maxDepth@ selects the engine's default. Throws
-- 'Hegel.Exception.HegelError' with 'HEGEL_E_INVALID_ARG' when the engine
-- rejects the grammar or its configuration.
compile ::
  Source ->
  Maybe Text ->
  Word64 ->
  Word32 ->
  Maybe (ForeignPtr HegelStringGenerator) ->
  IO (ForeignPtr HegelGrammar)
compile src start maxSize maxDepth alphabet =
  withContext \ctx -> do
    followHostUnicode
    result <- gather ctx src.rootText serve \builder ->
      String.withOptText start \startPtr ->
        withNullable alphabet \alphaPtr ->
          alloca \out ->
            -- Masked so that an async exception can't arrive between compiling
            -- the grammar and handing it to its finalizer, which would leak it.
            mask_ do
              hegel_grammar_new ctx builder startPtr maxSize maxDepth alphaPtr nullPtr 0 out >>= throwOnError ctx
              peek out >>= wrapGrammar
    either (\failure -> invariant ("libhegel rejected a grammar it accepted when loading: " <> failure.message)) pure result
  where
    serve path = maybe (invariant ("libhegel requested an unloaded grammar file " <> T.pack path)) pure (Map.lookup path src.imports)

    invariant :: Text -> IO a
    invariant detail = throwIO InvariantViolation {detail}

    withNullable :: Maybe (ForeignPtr HegelStringGenerator) -> (Ptr HegelStringGenerator -> IO a) -> IO a
    withNullable Nothing k = k nullPtr
    withNullable (Just fp) k = withForeignPtr fp k

-- | Wrap a compiled grammar in a GC-managed 'ForeignPtr' that frees it with a
-- throwaway context once every draw holding it has completed.
wrapGrammar :: Ptr HegelGrammar -> IO (ForeignPtr HegelGrammar)
wrapGrammar ptr = Concurrent.newForeignPtr ptr (withContext \ctx -> void (hegel_grammar_free ctx ptr))

-- | The content label of a grammar's root text and imports.
fingerprintOf :: Text -> Map FilePath (Maybe Text) -> Word64
fingerprintOf rootText imports =
  combineLabels (textLabel rootText : concatMap entry (Map.toAscList imports))
  where
    entry (path, contents) = [textLabel (T.pack path), maybe 0 textLabel contents]
    textLabel = labelFromName . TE.encodeUtf8

-- | Grammars written in [Lark's syntax](https://lark-parser.readthedocs.io/en/stable/grammar.html),
-- loaded for 'Hegel.Gen.grammar' to generate strings from.
--
-- > import Hegel.Grammar qualified as Grammar
-- >
-- > spec = do
-- >   calc <- runIO (Grammar.readFile "tests/grammars/calc.lark")
-- >   prop "parses every expression" (Gen.grammar calc & Gen.build) \src -> …
--
-- This module is designed for qualified import, since 'readFile' shadows the
-- "Prelude".
module Hegel.Grammar
  ( Source,
    readFile,
  )
where

import Control.Exception (throwIO, try)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.IO.Exception (IOErrorType (InappropriateType), IOException, ioe_type)
import GHC.Stack (HasCallStack, callStack)
import Hegel.Exception (Diagnostic (..), GrammarError (..))
import Hegel.Internal.Foreign.Raw (withContext)
import Hegel.Internal.Grammar (GatherFailure (..), Source (..), fingerprintOf, gather)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (readFile)

-- | Load a grammar from its root file, along with every file its @%import@s
-- reach.
--
-- The root path resolves against the working directory, and each imported
-- file resolves against the root's directory. An absolute import of Lark's
-- bundled @common@, @lark@, @python@, or @unicode@ grammar needs no file on
-- disk.
--
-- Throws t'GrammarError' naming the file for a Lark syntax error, a relative
-- import with no file, imports nested too deeply, or a file that isn't UTF-8,
-- and rethrows the 'IOException' when a file can't be read. An import cycle,
-- an undefined rule, or a rule that never terminates throws
-- 'Hegel.Exception.ValidationError' when the generator first draws.
readFile :: (HasCallStack) => FilePath -> IO Source
readFile root = do
  rootText <- readUtf8 root
  served <- newIORef Map.empty
  let serve path = do
        contents <- readImport path
        modifyIORef' served (Map.insert path contents)
        pure contents
  withContext (\ctx -> gather ctx rootText serve (\_ -> pure ())) >>= \case
    Left failure -> grammarError failure.file failure.message
    Right () -> do
      imports <- readIORef served
      pure Source {root, rootText, imports, fingerprint = fingerprintOf rootText imports}
  where
    readImport path = do
      let onDisk = takeDirectory root </> path
      try (BS.readFile onDisk) >>= \case
        Right bytes -> Just <$> decodeUtf8 path bytes
        Left err -> do
          missing <- isMissing onDisk err
          if missing then pure Nothing else throwIO err

    readUtf8 path = BS.readFile path >>= decodeUtf8 path

    decodeUtf8 path bytes = case TE.decodeUtf8' bytes of
      Right t -> pure t
      Left _ -> grammarError (Just path) "the file is not valid UTF-8"

    grammarError :: Maybe FilePath -> Text -> IO a
    grammarError path detail =
      throwIO . GrammarError $
        Diagnostic
          { context = T.pack root,
            detail,
            values = [("file", T.pack p) | Just p <- [path]],
            callStack = callStack
          }

-- | Whether a failed read means the file doesn't exist, which includes a path
-- through something that isn't a directory, but not a path naming a
-- directory.
isMissing :: FilePath -> IOException -> IO Bool
isMissing path err
  | isDoesNotExistError err = pure True
  | ioe_type err == InappropriateType = not <$> doesDirectoryExist path
  | otherwise = pure False

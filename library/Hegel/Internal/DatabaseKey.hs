-- | Deriving stable example-database keys from a test's identity.
--
-- A key is @"\<module\>:\<a/b/c\>/\<label\>"@: the call-site module (salt that
-- removes cross-module collisions and survives line edits), then the ancestor
-- describe path and the leaf label joined with @\/@ (mirroring hspec's
-- @--match@ path notation). Used by "Hegel.Hspec" and "Hegel.Tasty".
module Hegel.Internal.DatabaseKey
  ( propKey,
    moduleFromCallStack,
    joinPath,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)

-- | Build a database key from the call site, the ancestor describe path, and
-- the leaf label.
--
-- >>> propKey cs ["reverse"] "is involutive"   -- module "M"
-- "M:reverse/is involutive"
-- >>> propKey cs [] "is involutive"            -- module "M"
-- "M:is involutive"
propKey :: CallStack -> [String] -> String -> Text
propKey cs path label =
  moduleFromCallStack cs <> ":" <> joinPath (path <> [label])

-- | The defining module of the nearest call frame. Falls back to a fixed
-- sentinel when the stack is empty or frozen with no frames, so the key is
-- always well-defined.
moduleFromCallStack :: CallStack -> Text
moduleFromCallStack cs = case getCallStack cs of
  (_, loc) : _ -> T.pack loc.srcLocModule
  [] -> "<unknown-module>"

-- | Join path segments with @\/@, the separator hspec uses for test paths.
joinPath :: [String] -> Text
joinPath = T.intercalate "/" . map T.pack

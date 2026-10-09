-- | Deriving a test's example-database key and source location from its
-- identity.
--
-- A key is @"\<module\>:\<a/b/c\>/\<label\>"@: the call-site module (salt that
-- removes cross-module collisions and survives line edits), then the ancestor
-- describe path and the leaf label joined with @\/@ (mirroring hspec's
-- @--match@ path notation). Used by "Hegel.Hspec" and "Hegel.Tasty".
module Hegel.Internal.DatabaseKey
  ( propKey,
    moduleFromCallStack,
    joinPath,
    testLocationOf,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Hegel.Settings (TestLocation (..))

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

-- | The location of the test defined at the nearest call frame, named by its
-- describe path and leaf label joined as in 'propKey', or 'Nothing' when the
-- stack has no frames.
testLocationOf :: CallStack -> [String] -> String -> Maybe TestLocation
testLocationOf cs path label = case getCallStack cs of
  (_, loc) : _ ->
    Just
      TestLocation
        { file = T.pack loc.srcLocFile,
          line = loc.srcLocStartLine,
          scope = T.pack loc.srcLocModule,
          function = joinPath (path <> [label])
        }
  [] -> Nothing

-- | Output-encoding selection and 7-bit cleaning for rendered reports.
--
-- GHC derives an output handle's encoding from the locale; under @LANG=C@
-- (common in minimal containers) writing a box-drawing glyph or @✗@ does not
-- mojibake — it /throws/ (@hPutChar: invalid character@). The integrations
-- defend against that by detecting a non-UTF-capable handle and
-- transliterating the rendered report to 7-bit ASCII, rather than by forcing
-- the handle's encoding (mutating the host process's handle from a library is
-- too blunt — see @notes\/roadmap\/02-stateful-trace-rendering.md@).
--
-- This module owns the output 'Preference' (the never-crash decision) and the
-- text-cleaning pass, both independent of any glyph vocabulary. The stateful
-- event log's richer cell glyphs extend 'baseTransliterations' via
-- 'sevenBitCleanWith'; see "Hegel.Report.Glyph".
--
-- Designed for qualified import:
--
-- > import Hegel.Report.Encoding qualified as Encoding
module Hegel.Report.Encoding
  ( Preference (..),
    preference,
    cleanFor,
    sevenBitClean,
    sevenBitCleanWith,
    baseTransliterations,
  )
where

import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO (Handle, hGetEncoding)

-- | Which glyph regime the current output should get. Unicode is the default
-- everywhere, including CI (modern log viewers render box drawing fine); ascii
-- is the escape hatch for genuinely legacy consumers.
data Preference = PreferUnicode | PreferAscii
  deriving stock (Show, Eq)

-- | Choose a preference for output destined for the given handle. @HEGEL_GLYPHS@
-- (@ascii@ \/ @unicode@) overrides in both directions; otherwise a
-- non-UTF-capable encoding (e.g. @LANG=C@, where writing @✗@ would /throw/, not
-- mojibake) selects ascii — the never-crash requirement, answered by detection
-- rather than by forcing the handle's encoding. The caller names the handle it
-- is protecting (the integrations pass 'System.IO.stdout' as their best
-- knowledge of where the framework writes).
preference :: Handle -> IO Preference
preference h =
  lookupEnv "HEGEL_GLYPHS" >>= \case
    Just "ascii" -> pure PreferAscii
    Just "unicode" -> pure PreferUnicode
    _ -> do
      enc <- hGetEncoding h
      pure case enc of
        Just e | "UTF" `T.isInfixOf` T.pack (show e) -> PreferUnicode
        _ -> PreferAscii

-- | The text-cleaning pass a preference implies: 'sevenBitClean' for ascii
-- (the 7-bit guarantee covers user text too), identity otherwise.
cleanFor :: Preference -> Text -> Text
cleanFor = \case
  PreferAscii -> sevenBitClean
  PreferUnicode -> id

-- | Make a rendered report 7-bit clean using the base transliteration table:
-- transliterate the glyphs the (non-log) renderers are known to emit and
-- @\\xNNNN@-escape only what remains. See 'sevenBitCleanWith'.
sevenBitClean :: Text -> Text
sevenBitClean = sevenBitCleanWith baseTransliterations

-- | Make text 7-bit clean against a caller-supplied transliteration table:
-- keep ASCII as-is, transliterate any char in the table, and @\\xNNNN@-escape
-- every other non-ASCII char (genuinely unknown user text). The event log passes
-- @'baseTransliterations' <> its cell glyphs@ so chrome and log cell glyphs
-- are covered by one pass without turning the chrome into escape soup.
sevenBitCleanWith :: Map Char Text -> Text -> Text
sevenBitCleanWith transliterations = T.concatMap \c ->
  if Char.isAscii c
    then T.singleton c
    else case Map.lookup c transliterations of
      Just t -> t
      Nothing -> "\\x" <> T.pack (showHex (Char.ord c) "")

-- | The glyphs the base (non-log) renderers emit: source-splice borders,
-- the in-band failure mark, prose typography, and subscript digits. A
-- hand-maintained list (the one drift risk); the log's cell glyphs are
-- derived from its tables and unioned on top in "Hegel.Report.Glyph".
baseTransliterations :: Map Char Text
baseTransliterations =
  Map.fromList
    ( [ ('✗', "x"),
        ('┏', "+"),
        ('━', "-"),
        ('┃', "|"),
        ('│', "|"),
        ('⋮', ":"),
        ('·', "."),
        ('—', "--"),
        ('–', "-")
      ]
        <> [(toEnum (fromEnum '₀' + d), T.pack (show d)) | d <- [0 .. 9]]
    )

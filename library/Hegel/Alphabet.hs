-- | Curated character-alphabet presets for 'Hegel.Gen.Char.HasAlphabet',
-- so a caller reaches for @Alphabet.hexit@ instead of hand-building a
-- 'Hegel.Gen.Char.CharBuilder' for hexadecimals.
--
-- Intended to be imported with qualification:
--
-- > import Hegel.Alphabet qualified as Alphabet
--
-- Every preset is also an ordinary character generator on its own:
--
-- > Alphabet.hexit & Gen.build :: Gen Char
--
-- A preset is refined with the existing 'Hegel.Gen.Char.CharBuilder'
-- modifier vocabulary:
--
-- > Alphabet.alphaNum & Gen.excludeCharacters "lIO0"
--
-- 'only' and 'ranges' are the escape hatches for an alphabet with no named
-- preset.
--
-- A selection spanning more than one Unicode block, such as a script or a CJK
-- alphabet, can be constructed from 'ranges' directly:
--
-- > -- CJK Unified Ideographs plus Extension A
-- > Alphabet.ranges [(0x4E00, 0x9FFF), (0x3400, 0x4DBF)]
module Hegel.Alphabet
  ( -- * Escape hatches
    only,
    CodepointRange,
    ranges,

    -- * ASCII \/ Latin-1
    ascii,
    asciiPrintable,
    lower,
    upper,
    alpha,
    digit,
    alphaNum,
    binit,
    octit,
    hexit,
    latin1,
    unicode,
    asciiPunctuation,
    base64,
    base64Url,
    uriUnreserved,

    -- * Unicode
    whitespace,
    combiningMarks,
    zeroWidth,
    bidiControls,
  )
where

import Data.Char (GeneralCategory (..))
import Data.Char qualified as Char
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Hegel.Gen.Char (CharBuilder, Codec (..), categories, char, codec, includeCharacters, maxCodepoint, minCodepoint)

-- | An alphabet of exactly these characters and nothing else.
only :: Text -> CharBuilder
only cs = char & categories [] & includeCharacters cs

-- | A codepoint range, inclusive on both ends.
type CodepointRange = (Int, Int)

-- | An alphabet spanning exactly these codepoint ranges and nothing else.
--
-- Prefer 'Hegel.Gen.Char.minCodepoint'\/'Hegel.Gen.Char.maxCodepoint'
-- where possible; only reach for 'ranges' when the alphabet spans more than
-- one block, since a single min\/max pair can only express one contiguous
-- range.
ranges :: [CodepointRange] -> CharBuilder
ranges rs = char & categories [] & includeCharacters (T.pack (concatMap expand rs))
  where
    expand (lo, hi) = map Char.chr [lo .. hi]

-- | ASCII characters, codepoints @[0x00, 0x7F]@.
ascii :: CharBuilder
ascii = char & codec Ascii

-- | Printable ASCII characters, codepoints @[0x20, 0x7E]@.
asciiPrintable :: CharBuilder
asciiPrintable = char & minCodepoint 0x20 & maxCodepoint 0x7E

-- | Lowercase ASCII letters, codepoints @[0x61, 0x7A]@.
lower :: CharBuilder
lower = char & minCodepoint 0x61 & maxCodepoint 0x7A

-- | Uppercase ASCII letters, codepoints @[0x41, 0x5A]@.
upper :: CharBuilder
upper = char & minCodepoint 0x41 & maxCodepoint 0x5A

-- | ASCII letters, either case.
alpha :: CharBuilder
alpha = char & codec Ascii & categories [LowercaseLetter, UppercaseLetter]

-- | ASCII digits, codepoints @[0x30, 0x39]@.
digit :: CharBuilder
digit = char & minCodepoint 0x30 & maxCodepoint 0x39

-- | ASCII letters or digits.
alphaNum :: CharBuilder
alphaNum = char & codec Ascii & categories [LowercaseLetter, UppercaseLetter, DecimalNumber]

-- | Binary digits, @[\'0\', \'1\']@.
binit :: CharBuilder
binit = char & minCodepoint 0x30 & maxCodepoint 0x31

-- | Octal digits, @[\'0\', \'7\']@.
octit :: CharBuilder
octit = char & minCodepoint 0x30 & maxCodepoint 0x37

-- | Hexadecimal digits, both cases: @0@-@9@, @a@-@f@, @A@-@F@. Narrow to
-- one case with 'Hegel.Gen.Char.excludeCharacters'.
hexit :: CharBuilder
hexit = only "0123456789abcdefABCDEF"

-- | Latin-1 characters, codepoints @[0x00, 0xFF]@.
latin1 :: CharBuilder
latin1 = char & codec Latin1

-- | All of Unicode, with surrogates excluded; an alias for 'Hegel.Gen.Char.char'.
unicode :: CharBuilder
unicode = char

-- | The 32 printable ASCII characters that are neither letters, digits, nor
-- space, spanning both the Unicode punctuation and symbol categories.
asciiPunctuation :: CharBuilder
asciiPunctuation = only "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | The RFC 4648 §4 base64 alphabet, without the @=@ padding character.
base64 :: CharBuilder
base64 = only "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | The RFC 4648 §5 base64url alphabet, without the @=@ padding character.
base64Url :: CharBuilder
base64Url = only "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

-- | The RFC 3986 §2.3 unreserved character set for a URI.
uriUnreserved :: CharBuilder
uriUnreserved = only "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"

-- | Unicode whitespace: the space, line-separator, and paragraph-separator
-- categories, plus the ASCII control characters conventionally treated as
-- whitespace.
whitespace :: CharBuilder
whitespace = char & categories [Space, LineSeparator, ParagraphSeparator] & includeCharacters "\t\n\v\f\r\x85"

-- | Unicode combining marks: non-spacing, spacing-combining, and enclosing.
combiningMarks :: CharBuilder
combiningMarks = char & categories [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

-- | Zero-width characters: zero-width space, non-joiner, joiner, word
-- joiner, and byte-order mark.
zeroWidth :: CharBuilder
zeroWidth = only "\x200B\x200C\x200D\x2060\xFEFF"

-- | Bidirectional-control characters: the left-to-right and right-to-left
-- marks, the explicit embedding and override controls, and the isolate
-- controls.
bidiControls :: CharBuilder
bidiControls = only "\x200E\x200F\x202A\x202B\x202C\x202D\x202E\x2066\x2067\x2068\x2069"

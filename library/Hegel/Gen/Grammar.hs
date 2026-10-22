-- | 'Text' generator for strings a Lark grammar derives, loaded with
-- 'Hegel.Grammar.readFile'.
--
-- > calc <- Grammar.readFile "tests/grammars/calc.lark"
-- > Gen.grammar calc & Gen.maxSize 200 & Gen.build
--
-- A drawn string shrinks through its derivation, so a counterexample
-- shrinks toward the simplest string the grammar allows.
module Hegel.Gen.Grammar
  ( -- * Builder
    GrammarBuilder,
    grammar,

    -- * Modifiers
    startSymbol,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign (ForeignPtr)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hegel.Exception (Diagnostic (..), ValidationError (..))
import Hegel.Gen.Builder (Build (..), HasDepth (..), HasMaxSize (..), engineRejection)
import Hegel.Gen.Char (CharBuilder, HasAlphabet (..), buildCharTextGen)
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (HegelGrammar, Label (LabelGrammar), combineLabels, drawGrammar, labelFromName, spanLabel)
import Hegel.Internal.Grammar (Source (..), compile)
import System.IO.Unsafe (unsafePerformIO)

-- | Builder for a grammar-derived 'Text' generator, bounded with
-- 'HasMaxSize' and 'HasDepth' and restricted with 'HasAlphabet'.
data GrammarBuilder = GrammarBuilder
  { gSource :: !Source,
    gStartSymbol :: !(Maybe Text),
    gMaxSize :: !(Maybe Int),
    gMaxDepth :: !(Maybe Int),
    gAlphabet :: !(Maybe CharBuilder)
  }

-- | Generate strings the grammar derives from its @start@ rule.
grammar :: Source -> GrammarBuilder
grammar src =
  GrammarBuilder {gSource = src, gStartSymbol = Nothing, gMaxSize = Nothing, gMaxDepth = Nothing, gAlphabet = Nothing}

-- | Derive strings from the named rule instead of @start@.
startSymbol :: Text -> GrammarBuilder -> GrammarBuilder
startSymbol s b = b {gStartSymbol = Just s}

-- | Cap every string at this many characters, which must be in
-- @[1, 1048576]@ and at least the length of the shortest string the start
-- rule derives. The default is 8192.
instance HasMaxSize GrammarBuilder where
  maxSize n b = b {gMaxSize = Just n}

-- | Cap how deeply rules nest in a derivation, which must be in @[1, 500]@
-- and at least the shallowest nesting the start rule needs. The default is
-- 200.
instance HasDepth GrammarBuilder where
  maxDepth n b = b {gMaxDepth = Just n}

-- | Draw every character of a string from the given 'CharBuilder', which
-- must be able to produce every terminal of the grammar.
instance HasAlphabet GrammarBuilder where
  alphabet cb b = b {gAlphabet = Just cb}

instance Build GrammarBuilder Text where
  build b = withFrozenCallStack $ Draw label \tc -> do
    traverse_ (checkRange "maxSize" 1048576) b.gMaxSize
    traverse_ (checkRange "maxDepth" 500) b.gMaxDepth
    -- The engine can also reject the grammar at draw time, when no string fits
    -- within its size cap and the work a draw is allowed.
    engineRejection "Hegel.Gen.Grammar" (drawGrammar tc compiled)
    where
      label =
        combineLabels
          [ spanLabel LabelGrammar,
            b.gSource.fingerprint,
            labelFromName (TE.encodeUtf8 (fromMaybe "start" b.gStartSymbol))
          ]

      -- Bound here, not inside the 'Draw' lambda, so the grammar compiles once
      -- and every draw of this 'Gen' value shares it.
      compiled :: ForeignPtr HegelGrammar
      compiled = unsafePerformIO (withFrozenCallStack (engineRejection "Hegel.Gen.Grammar" compileGrammar))
      {-# NOINLINE compiled #-}

      -- The engine keeps its own copy of the alphabet's character set, so
      -- the alphabet handle doesn't need to outlive compilation.
      compileGrammar = do
        alpha <- traverse (buildCharTextGen 1 1) b.gAlphabet
        compile b.gSource b.gStartSymbol (maybe 0 fromIntegral b.gMaxSize) (maybe 0 fromIntegral b.gMaxDepth) alpha

checkRange :: (HasCallStack) => Text -> Int -> Int -> IO ()
checkRange field hi n =
  unless (n >= 1 && n <= hi) $
    throwIO
      ( ValidationError
          Diagnostic
            { context = "Hegel.Gen.Grammar",
              detail = field <> " must be in [1, " <> T.pack (show hi) <> "]",
              values = [(field, T.pack (show n))],
              callStack = callStack
            }
      )

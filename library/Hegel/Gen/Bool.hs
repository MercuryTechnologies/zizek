-- | Boolean generator.
--
-- > Gen.bool                    & Gen.build   -- fair coin
-- > Gen.bool & Gen.weighted 0.9 & Gen.build   -- 'True' ~90% of the time
module Hegel.Gen.Bool
  ( BoolBuilder,
    bool,
    weighted,
  )
where

import CBOR.Value (Value (..))
import Hegel.Cbor (ParseError (..))
import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (Gen (..), basic)
import Hegel.Schema qualified as Schema
import Hegel.TestCase (primitiveBoolean)

-- | A boolean generator. The default ('bool') is a fair coin satisfied in a
-- single schema round-trip; 'weighted' switches it to the @primitive_boolean@
-- draw, which biases the result.
newtype BoolBuilder = BoolBuilder
  { -- | Probability of drawing 'True'. 'Nothing' is a fair coin.
    probability :: Maybe Double
  }

-- | Generate a random boolean.
bool :: BoolBuilder
bool = BoolBuilder {probability = Nothing}

-- | Bias the draw toward 'True' with the given probability (clamped to
-- @[0,1]@ by the engine).
weighted :: Double -> BoolBuilder -> BoolBuilder
weighted p b = b {probability = Just p}

instance Build BoolBuilder Bool where
  build b = case b.probability of
    -- A plain fair coin collapses to a single schema request.
    Nothing -> basic Schema.bool parseBool
    -- Any bias uses the dedicated primitive draw.
    Just p -> Draw \tc -> primitiveBoolean tc p

parseBool :: Value -> Either ParseError Bool
parseBool (Bool b) = Right b
parseBool v = Left ParseError {expected = "boolean", got = v}

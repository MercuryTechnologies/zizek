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

import Hegel.Gen.Builder (Build (..))
import Hegel.Gen.Internal (Gen (..))
import Hegel.Internal.DataSource (drawBool)

-- | A boolean generator. The default ('bool') is a fair coin (@p = 0.5@);
-- 'weighted' biases the draw.
newtype BoolBuilder = BoolBuilder
  { -- | Probability of drawing 'True'.
    probability :: Double
  }

-- | Generate a random boolean.
bool :: BoolBuilder
bool = BoolBuilder {probability = 0.5}

-- | Bias the draw toward 'True' with the given probability (clamped to
-- @[0,1]@ by the engine).
weighted :: Double -> BoolBuilder -> BoolBuilder
weighted p b = b {probability = p}

instance Build BoolBuilder Bool where
  build b = Draw \tc -> drawBool tc b.probability

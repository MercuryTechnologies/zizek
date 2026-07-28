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

import Control.Exception (throwIO)
import Data.Text qualified as T
import Hegel.Gen.Builder (Build (..), ValidationError (..))
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

-- | Bias the draw toward 'True' with the given probability, which must be in
-- @[0,1]@ and not NaN.
weighted :: Double -> BoolBuilder -> BoolBuilder
weighted p b = b {probability = p}

instance Build BoolBuilder Bool where
  build b = Draw \tc -> do
    checkProbability b.probability
    drawBool tc b.probability

-- | Require @p@ to be a valid probability: in @[0,1]@ and not NaN.
checkProbability :: Double -> IO ()
checkProbability p
  | isNaN p || p < 0 || p > 1 =
      throwIO
        ValidationError
          { context = "Hegel.Gen.Bool",
            detail = "probability (" <> T.pack (show p) <> ") outside [0, 1]"
          }
  | otherwise = pure ()

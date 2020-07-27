module Beans.Amount
  ( Amount,
    showFixed,
    asFloat,
  )
where

import Data.Fixed (Fixed, Micro)
import qualified Data.Fixed as F
import Data.Monoid (Sum (Sum))
import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc (Pretty (pretty))

newtype Amount = Amount Micro
  deriving (Eq, Ord, Show, Num, Real, RealFrac, Fractional)
  deriving (Semigroup, Monoid) via (Sum Amount)

instance Pretty Amount where
  pretty = pretty . showRounded 2

showFixed :: Amount -> Text
showFixed (Amount n) = pack $ F.showFixed True n

roundTo :: F.HasResolution a => Int -> Fixed a -> Fixed a
roundTo digits n = fromIntegral (round (n * factor) :: Integer) / factor
  where
    factor = fromIntegral (10 ^ digits :: Integer)

showRounded :: Int -> Amount -> String
showRounded d (Amount nbr)
  | d == 0 = take (x - 1) s
  | otherwise = take x s
  where
    s = F.showFixed False (roundTo d nbr)
    x = length s - precision + d
    precision = intLog10 (F.resolution nbr)

intLog10 :: Integer -> Int
intLog10 n
  | n < 10 = 0
  | otherwise = 1 + intLog10 (n `div` 10)

toFloat :: Fractional a => Amount -> a
toFloat = fromRational . toRational

fromFloat :: Real a => a -> Amount
fromFloat = fromRational . toRational

asFloat :: (Fractional a, Real a) => (a -> a) -> Amount -> Amount
asFloat f = fromFloat . f . toFloat

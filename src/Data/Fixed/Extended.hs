module Data.Fixed.Extended
  ( module Data.Fixed
  , showRounded
  , roundTo
  )
where

import           Data.Fixed

roundTo :: HasResolution a => Int -> Fixed a -> Fixed a
roundTo digits n = fromIntegral r / fromIntegral factor
 where
  r      = round (n * fromIntegral factor) :: Integer
  factor = 10 ^ digits :: Integer

showRounded :: HasResolution a => Int -> Fixed a -> String
showRounded d nbr | d == 0    = take (x - 1) s
                  | otherwise = take x s
 where
  s         = showFixed False (roundTo d nbr)
  x         = length s - precision + d
  precision = intLog10 (resolution nbr)

intLog10 :: Integer -> Int
intLog10 n | n < 10 = 0
intLog10 n          = 1 + intLog10 (n `div` 10)

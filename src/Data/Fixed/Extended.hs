module Data.Fixed.Extended
  ( module Data.Fixed
  , showRounded
  , roundTo
  )
where

import           Data.Fixed


-- only works for positive a
showIntegerZeros :: Int -> Integer -> String
showIntegerZeros n      0 = replicate n '0'
showIntegerZeros digits a = replicate (digits - length s) '0' ++ s
  where s = show a

withDot :: String -> String
withDot "" = ""
withDot s  = '.' : s

showRounded :: (HasResolution a) => Int -> Fixed a -> String
showRounded digits fa = sign ++ show i ++ withDot
  (take digits $ showIntegerZeros digits' d)
 where
  sign      = if fa < 0 then "-" else ""
  MkFixed a = roundTo digits fa
  res       = resolution fa
  (i, d)    = divMod (abs a) res
  digits'   = ceiling (logBase 10 (fromInteger res) :: Double)

roundTo :: HasResolution a => Int -> Fixed a -> Fixed a
roundTo digits n = fromIntegral r / fromIntegral factor
 where
  r :: Integer = round (n * fromIntegral factor)
  factor       = 10 ^ digits :: Integer

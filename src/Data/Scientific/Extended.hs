module Data.Scientific.Extended (module Data.Scientific, sdiv) where

import           Data.Scientific

sdiv :: Scientific -> Scientific -> Scientific
sdiv x y = fromFloatDigits (toRealFloat x / toRealFloat y :: Double)

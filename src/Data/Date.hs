module Data.Date
  ( Date
  , fromGregorian
  ) where

import Data.Text.Prettyprint.Doc (Pretty, pretty)
import qualified Data.Time.Calendar as C

newtype Date = Date
  { _day :: C.Day
  } deriving (Eq, Show)

instance Pretty Date where
  pretty = pretty . show . _day

fromGregorian :: Integer -> Int -> Int -> Date
fromGregorian y m d = Date (C.fromGregorian y m d)

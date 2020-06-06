module Beans.Date
  ( Date (Date),
    Interval (..),
    addDays,
    fromGregorian,
    isBetween,
    partition,
  )
where

import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as Calendar
import Data.Time.Format.ISO8601 (iso8601Show)

newtype Date = Date Day deriving (Eq, Ord, FromJSON)

instance Show Date where
  show (Date d) = iso8601Show d

instance Pretty Date where
  pretty (Date d) = pretty . iso8601Show $ d

type Greg = (Integer, Int, Int)

fromGregorian :: (Integer, Int, Int) -> Date
fromGregorian (y, m, d) = Date $ Calendar.fromGregorian y m d

toGregorian :: Date -> (Integer, Int, Int)
toGregorian = Calendar.toGregorian . coerce

isBetween :: Date -> Date -> Date -> Bool
isBetween dateFrom dateTo d = dateFrom <= d && d <= dateTo

transform :: (Greg -> Greg) -> Date -> Date
transform f = fromGregorian . f . toGregorian

addDays :: Integer -> Date -> Date
addDays n (Date d) = Date $ Calendar.addDays n d

data Interval = Daily | Weekly | Monthly | Quarterly | Yearly deriving (Eq, Show)

startOf :: Interval -> Date -> Date
startOf Daily = id
startOf Weekly = \(Date d) ->
  Date $
    Calendar.addDays (fromIntegral (- (((fromEnum . Calendar.dayOfWeek $ d) - 1) `mod` 7))) d
startOf Monthly = transform (\(y, m, _) -> (y, m, 1))
startOf Quarterly = transform (\(y, m, _) -> (y, ((m - 1) `div` 3 + 1) * 3 - 2, 1))
startOf Yearly = transform (\(y, _, _) -> (y, 1, 1))

endOf :: Interval -> Date -> Date
endOf Daily = transform id
endOf Weekly = \(Date d) ->
  Date $
    Calendar.addDays (fromIntegral $ 6 - (((fromEnum . Calendar.dayOfWeek $ d) - 1) `mod` 7)) d
endOf Monthly = transform (\(y, m, _) -> (y, m, 31))
endOf Quarterly = transform (\(y, m, _) -> (y, ((m - 1) `div` 3 + 1) * 3, 31))
endOf Yearly = transform (\(y, _, _) -> (y, 12, 31))

partition :: Interval -> Date -> Date -> [Date]
partition interval t0 t1 = addDays (-1) (startOf interval t0) : partition' t0 t1
  where
    partition' :: Date -> Date -> [Date]
    partition' d0 d1
      | d0 > d1 = []
      | otherwise = end : partition' (addDays 1 end) d1
      where
        end = endOf interval d0

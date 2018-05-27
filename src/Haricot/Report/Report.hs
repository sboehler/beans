module Haricot.Report.Report
  (Report(..), createReports, Entry(..))
where

import           Data.Bifunctor           (second)
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (Scientific)
import           Data.Text.Lazy           (Text)
import           Haricot.Accounts         (Key (..))

data Entry = Entry
  { _labels :: [Text]
  , _entry  :: (Key, Scientific)
  } deriving (Eq, Ord)

data Report
  = Group Text
          [Report]
  | Single Entry

createReports :: [Entry] -> [Report]
createReports entries =
  let grouped = entries `groupBy` classify
      reports = createReport <$> grouped
   in concat reports

createReport :: (Maybe Text, [Entry]) -> [Report]
createReport (Just n, es)  = [Group n (createReports es)]
createReport (Nothing, es) = Single <$> es

classify :: Entry -> (Maybe Text, Entry)
classify (Entry (s:ss) e) = (Just s, Entry ss e)
classify (Entry [] e)     = (Nothing, Entry [] e)

groupBy :: (Ord k) => [a] -> (a -> (k, v)) -> [(k, [v])]
groupBy list key = M.toList $ M.fromListWith (++) (second pure . key <$> list)

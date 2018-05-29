module Haricot.Report.Report
  (Report(..), Entry(..), createReport)
where

import           Data.Bifunctor           (second)
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (Scientific)
import           Data.Text                (Text)
import           Haricot.AST              (CommodityName (..), Lot (..))


newtype Report = Report [Entry]

data Position = Position CommodityName Lot Scientific
  deriving (Eq, Ord)

type Item = ([Text], [Position])

data Entry
  = SingleEntry Item
  | GroupEntry Text
               [Entry]
  deriving (Eq, Ord)

createReport :: [Item] -> Report
createReport = Report . groupEntries

groupEntries :: [Item] -> [Entry]
groupEntries = concatMap convert . groupWith split

convert :: (Maybe Text, [Item]) -> [Entry]
convert (Just n, [(_, positions)]) = [SingleEntry ([n], positions)]
convert (Just n, items)            = [GroupEntry n (groupEntries items)]
convert (Nothing, items)           = SingleEntry <$> items

split :: Item -> (Maybe Text, Item)
split (n:ns, positions) = (Just n, (ns, positions))
split ([], positions)   = (Nothing, ([], positions))

groupWith :: (Ord k) => (a -> (k, v)) -> [a] -> [(k, [v])]
groupWith f l = M.toList $ M.fromListWith (++) (second pure . f <$> l)

-- firstCol :: Report -> [String]
-- firstCol (Group t reports) =
--   unpack t : (indent 2 <$> concat (firstCol <$> reports))
-- firstCol (Single Entry {..}) = [show _labels]

-- indent :: Int -> String -> String
-- indent n = (++ replicate n ' ')

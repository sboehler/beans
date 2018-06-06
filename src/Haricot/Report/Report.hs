module Haricot.Report.Report
  (Section(..), createReport)
where

import           Data.Bifunctor           (second)
import qualified Data.Map.Strict.Extended as M
import qualified Data.List as L
import           Data.Scientific.Extended (Scientific)
import           Data.Text                (Text)
import           Haricot.AST              (CommodityName (..), Lot (..))

data Section =
  Section Text
          [Position]
          [Section]
  deriving (Eq, Ord)

type Item = ([Text], [Position])

data Position = Position CommodityName Lot Scientific
  deriving (Eq, Ord)

createReport :: [Item] -> Section
createReport = groupItems "Report"

groupItems :: Text -> [Item] -> Section
groupItems title items =
  let (topItems, otherItems) = L.partition (null . fst) items
      positions = concatMap snd topItems
      subSections = groupWith splitItem otherItems
   in Section title positions (M.elems $ M.mapWithKey groupItems subSections)

splitItem :: Item -> (Text, Item)
splitItem (n:ns, positions) = (n, (ns, positions))
splitItem ([], positions)   = (mempty, ([], positions))

groupWith :: (Ord k) => (a -> (k, v)) -> [a] -> M.Map k [v]
groupWith f l = M.fromListWith (++) (second pure . f <$> l)

-- firstCol :: Report -> [String]
-- firstCol (Group t reports) =
--   unpack t : (indent 2 <$> concat (firstCol <$> reports))
-- firstCol (Single Entry {..}) = [show _labels]

-- printReport :: Report -> [[String]]
-- printReport (Report entries) = concatMap (entryRows 0) entries

-- entryRows :: Int -> Section -> [[ String ]]
-- entryRows n (SingleEntry (labels, positions)) =
--   let cols = toRow <$> positions
--       firstCol = indent n <$> show labels : repeat ""
--    in zipWith (:) firstCol cols
-- entryRows n (GroupEntry label entries) =
--   let m = concatMap (entryRows (n + 2)) entries
--    in [show label, "", "", ""] : m

-- toRow :: Position -> [String]
-- toRow (Position c l s) = [show c, show l, show s]


-- indent :: Int -> String -> String
-- indent n = (++ replicate n ' ')

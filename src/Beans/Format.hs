module Beans.Format
  (Section(..), formatTable, reportToRows, createReport)
where

import           Beans.Data.Accounts (AccountName (..), Accounts, Amount,
                                      CommodityName (..), Lot (..), toList)
import qualified Beans.Data.Map      as M
import Data.Monoid ((<>))
import           Beans.Table         (ColDesc (..), formatStandard, left, right,
                                      showTable)
import qualified Data.List           as L
import           Data.Text.Lazy      (Text, intercalate, pack, unpack)

data Report = Report Positions [Section] Positions deriving (Show)

type Positions = M.Map (CommodityName, Maybe Lot) Amount

data Section = Section
  { _labels    :: [Text]
  , _positions :: Positions
  , _sections  :: [Section]
  , _subtotals :: Positions
  } deriving (Show)


-- Creating a report

createReport :: Accounts -> Report
createReport = toReport . groupSections "" . toSections
  where
    toSections = fmap toSection . toList
    toSection ((a, c, l), s) =
      let pos = M.singleton (c, l) s
       in Section (toLabel a) pos [] mempty
    toLabel (AccountName t ns) = pack (show t) : ns
    toReport (Section _ pos sec sub) = Report pos sec sub

groupSections :: Text -> [Section] -> Section
groupSections title sections =
  let (rootSections, childSections) = L.partition (null . _labels) sections
      positions = mconcat $ _positions <$> rootSections
      subsections = group (splitSection <$> childSections)
      subtotals = positions <> mconcat (_subtotals <$> subsections)
   in Section [title] positions subsections subtotals

group :: [(Text, [Section])] -> [Section]
group = fmap (uncurry groupSections) . M.toList . M.fromList

splitSection :: Section -> (Text, [Section])
splitSection (Section (n:ns) ps ss st) = (n, [Section ns ps ss st])
splitSection (Section [] ps ss st)     = (mempty, [Section [] ps ss st])


-- Formatting a report into rows

reportToRows :: Report -> [[String]]
reportToRows (Report p s st) = positions ++ sections ++ subtotals
  where
    positions = positionsToRows "" p
    sections = concatMap sectionToRows s
    subtotals = positionsToRows "Total:" st

sectionToRows :: Section -> [[String]]
sectionToRows (Section t ps ss st) =
  positions ++ (indentFirstColumn 2 <$> subsections) ++ subtotals
  where
    title = intercalate ":" t
    positions = positionsToRows title ps
    subsections = concatMap sectionToRows ss
    subtotals =
      if null ss
        then []
        else positionsToRows (title <> ":Total") st

positionsToRows :: Text -> Positions -> [[String]]
positionsToRows title ps =
  case ls of
    [] -> [[unpack title, "", "", ""]]
    _  -> zipWith (:) (unpack title : repeat "") ls
  where
    ls = line <$> M.toList ps
    line ((c, l), s) = [show c, maybe "" show l, formatStandard s]

indentFirstColumn :: Int -> [String] -> [String]
indentFirstColumn n (s:ss) = (replicate n ' '++s):ss
indentFirstColumn _ []     = []


-- formatting rows into a table
formatTable :: [[String]] -> String
formatTable t =
  showTable
    [ ColDesc left "Account" left (!! 0)
    , ColDesc left "Amount" right (!! 3)
    , ColDesc left "Commodity" left (!! 1)
    , ColDesc left "Lot" left (!! 2)
    ]
    t
    []

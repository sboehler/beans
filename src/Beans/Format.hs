module Beans.Format
  (Section(..), formatTable, reportToRows, createReport)
where

import           Beans.AST                (AccountName (..), CommodityName (..),
                                           Lot (..))
import           Beans.Data.Accounts      (Accounts, toList)
import           Beans.Table              (ColDesc (..), formatStandard, left,
                                           right, showTable)
import qualified Data.List                as L
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (Scientific)
import           Data.Text.Lazy           (Text, intercalate, pack, unpack)

data Report = Report [Position] [Section] deriving (Show)

data Section = Section
  { _labels    :: [Text]
  , _positions :: [Position]
  , _sections  :: [Section]
  } deriving (Show)

data Position = Position CommodityName (Maybe Lot) Scientific
  deriving (Show)


-- Creating a report

createReport :: Accounts -> Report
createReport = toReport . groupSections "" . toSections
  where
    toSections = fmap toSection . toList
    toSection ((a, c, l), s) = Section (toLabel a) [Position c l s] []
    toLabel (AccountName t ns) = pack (show t) : ns
    toReport (Section _ pos sec) = Report pos sec

groupSections :: Text -> [Section] -> Section
groupSections title sections =
  let (rootSections, childSections) = L.partition (null . _labels) sections
      positions = concatMap _positions rootSections
      subsections = group (splitSection <$> childSections)
   in Section [title] positions subsections

group :: [(Text, [Section])] -> [Section]
group = M.elems . M.mapWithKey groupSections . M.fromListWith (++)

splitSection :: Section -> (Text, [Section])
splitSection (Section (n:ns) ps ss) = (n, [Section ns ps ss])
splitSection (Section [] ps ss)     = (mempty, [Section [] ps ss])


-- Formatting a report into rows

reportToRows :: Report -> [[String]]
reportToRows (Report p s) = positions ++ sections
  where
    positions = positionsToRows "" p
    sections = concatMap sectionToRows s

sectionToRows :: Section -> [[String]]
sectionToRows (Section t ps ss) =
  positions ++ (indentFirstColumn 2 <$> subsections)
  where
    title = intercalate ":" t
    positions = positionsToRows title ps
    subsections = concatMap sectionToRows ss

positionsToRows :: Text -> [Position] -> [[String]]
positionsToRows title (Position c l s:ps) = line : positionsToRows "" ps
  where
      line = [unpack title, show c, maybe "" show l, formatStandard s]
positionsToRows "" [] = []
positionsToRows title [] = [[unpack title, "", "", ""]]

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

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
import           Data.Text.Lazy      (Text, pack, unpack)

type Positions = M.Map (CommodityName, Maybe Lot) Amount

data Section = Section
  { 
   _positions :: Positions
  , _sections  :: M.Map Text Section
  , _subtotals :: Positions
  } deriving (Show)


-- Creating a report

createReport :: Accounts -> Section
createReport = groupSections . fmap toItem . toList
  where
    toItem ((a, c, l), s) =
      let pos = M.singleton (c, l) s
       in (toLabel a, pos)
    toLabel (AccountName t ns) = pack (show t) : ns

groupSections :: [([Text], Positions)] -> Section
groupSections items =
  let (rootSections, childSections) = L.partition (null . fst) items
      positions = mconcat $ snd <$> rootSections
      subsections = groupSections <$> M.fromList (splitSection <$> childSections)
      subtotals = positions <> mconcat (_subtotals . snd <$> M.toList subsections)
   in Section  positions subsections subtotals

splitSection :: ([Text], Positions) -> (Text, [([Text], Positions)])
splitSection (n:ns, ps) = (n, [(ns, ps)])
splitSection ([], ps) = (mempty, [([], ps)])


-- Formatting a report into rows

reportToRows :: Section -> [[String]]
reportToRows t =  sectionToRows ("", t)

sectionToRows :: (Text, Section) -> [[String]]
sectionToRows (t, Section ps ss st) =
  positions ++ (indentFirstColumn 2 <$> subsections)
  where
    subsections = concatMap sectionToRows (M.toList ss)
    st' =
      if null subsections
        then mempty
        else st
    positions = positionsToRows t ps st'

positionsToRows :: Text -> Positions -> Positions -> [[String]]
positionsToRows title ps st =
  case combined of
    [] -> [[unpack title, "", "", "", ""]]
    _ -> zipWith (:) (unpack title : repeat "") combined
  where
    combined =
      line <$>
      M.toList (M.accumulate (formatStandard <$> st) (formatStandard <$> ps))
    line ((c, l), s) = s ++ [show c, maybe "" show l]

indentFirstColumn :: Int -> [String] -> [String]
indentFirstColumn n (s:ss) = (replicate n ' '++s):ss
indentFirstColumn _ []     = []


-- formatting rows into a table
formatTable :: [[String]] -> String
formatTable t =
  showTable
    [ ColDesc left "Account" left (!! 0)
    , ColDesc left "Total" right (!! 1)
    , ColDesc left "Amount" right (!! 2)
    , ColDesc left "Commodity" left (!! 3)
    , ColDesc left "Lot" left (!! 4)
    ]
    t
    []

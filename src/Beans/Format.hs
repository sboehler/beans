module Beans.Format
  ( Section(..)
  , formatTable
  , reportToRows
  , createHierarchicalReport
  , createFlatReport
  ) where

import           Beans.Data.Accounts       (AccountName (..), Accounts, Amount,
                                            CommodityName (..), Lot (..))
import qualified Beans.Data.Map            as M
import           Beans.Pretty              ()
import           Beans.Table               (Column (..), formatStandard, left,
                                            right, showTable)
import           Control.Applicative       (ZipList (..))
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (pretty)

type Positions = M.Map (CommodityName, Maybe Lot) Amount

data Section = Section
  { sPositions :: Positions
  , sSections  :: M.Map Text Section
  , sSubtotals :: Positions
  } deriving (Show)

-- Creating a report
createHierarchicalReport :: Accounts -> Section
createHierarchicalReport = groupSections . fmap toItem . M.toList
  where
    toItem ((a, c, l), s) =
      let pos = M.singleton (c, l) s
       in (toLabel a, pos)
    toLabel (AccountName t ns) = T.pack (show t) : ns

groupSections :: [([Text], Positions)] -> Section
groupSections items =
  let (rootSections, childSections) = L.partition (null . fst) items
      positions = mconcat $ snd <$> rootSections
      subsections =
        groupSections <$> M.fromListM (splitSection <$> childSections)
      subtotals =
        positions <> mconcat (sSubtotals . snd <$> M.toList subsections)
   in Section positions subsections subtotals

splitSection :: ([Text], Positions) -> (Text, [([Text], Positions)])
splitSection (n:ns, ps) = (n, [(ns, ps)])
splitSection ([], ps)   = (mempty, [([], ps)])

createFlatReport :: Accounts -> Section
createFlatReport = groupSections2 . fmap toItem . M.toList
  where
    toItem ((a, c, l), s) =
      let pos = M.singleton (c, l) s
       in (toLabel a, pos)
    toLabel = T.pack . show

groupSections2 :: [(Text, Positions)] -> Section
groupSections2 items =
  let groupedItems = M.fromListM items
      subsections = (\p -> Section p M.empty p) <$> groupedItems
      subtotals = mconcat (sSubtotals . snd <$> M.toList subsections)
   in Section mempty subsections subtotals

-- Formatting a report into rows
data Row = Row
  { rAccount   :: Maybe Text
  , rTotal     :: Maybe Amount
  , rAmount    :: Maybe Amount
  , rCommodity :: Maybe CommodityName
  , rLot       :: Maybe Lot
  }

reportToRows :: Section -> [Row]
reportToRows t = sectionToRows ("", t)

sectionToRows :: (Text, Section) -> [Row]
sectionToRows (t, Section ps ss st) = positions ++ subsections
  where
    subsections = indent 2 <$> (sectionToRows =<< M.toList ss)
    subtotals =
      case subsections of
        [] -> mempty
        _  -> st
    positions = positionsToRows t ps subtotals

positionsToRows :: Text -> Positions -> Positions -> [Row]
positionsToRows title positions subtotals =
  case M.toList (M.combineM [subtotals, positions]) of
    [] -> [Row (Just title) Nothing Nothing Nothing Nothing]
    c ->
      getZipList $ do
        rAccount <- ZipList (Just title : repeat Nothing)
        rTotal <- Just . (!! 0) . snd <$> ZipList c
        rAmount <- Just . (!! 1) . snd <$> ZipList c
        rCommodity <- Just . fst . fst <$> ZipList c
        rLot <- snd . fst <$> ZipList c
        pure Row {..}

indent :: Int -> Row -> Row
indent n (Row first a b c d) = Row (indent' <$> first) a b c d
  where
    indent' t = T.replicate (fromIntegral n) " " <> t

-- formatting rows into a table
formatTable :: [Row] -> Text
formatTable =
  showTable
    [ Column left "Account" left (fromMaybe "" . rAccount)
    , Column left "Total" right (maybe "" formatStandard . rTotal)
    , Column left "Amount" right (maybe "" formatStandard . rAmount)
    , Column left "Commodity" left (T.pack . maybe "" show . rCommodity)
    , Column left "Lot" left (T.pack . maybe "" (show . pretty) . rLot)
    ]

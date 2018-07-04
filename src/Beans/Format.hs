module Beans.Format
  (Section(..), formatTable, reportToRows, createReport)
where

import           Beans.Data.Accounts (AccountName (..), Accounts, Amount,
                                      CommodityName (..), Lot (..), toList)
import qualified Beans.Data.Map      as M
import           Beans.Table         (ColDesc (..), formatStandard, left, right,
                                      showTable)
import           Control.Applicative (ZipList (..))
import qualified Data.List           as L
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T

type Positions = M.Map (CommodityName, Maybe Lot) Amount

data Section = Section
  {
   _positions  :: Positions
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
    toLabel (AccountName t ns) = T.pack (show t) : ns

groupSections :: [([Text], Positions)] -> Section
groupSections items =
  let (rootSections, childSections) = L.partition (null . fst) items
      positions = mconcat $ snd <$> rootSections
      subsections = groupSections <$> M.fromList (splitSection <$> childSections)
      subtotals = positions <> mconcat (_subtotals . snd <$> M.toList subsections)
   in Section  positions subsections subtotals

splitSection :: ([Text], Positions) -> (Text, [([Text], Positions)])
splitSection (n:ns, ps) = (n, [(ns, ps)])
splitSection ([], ps)   = (mempty, [([], ps)])


-- Formatting a report into rows

data Row = Row {
  _account   :: Maybe Text,
  _total     :: Maybe Amount,
  _amount    :: Maybe Amount,
  _commodity :: Maybe CommodityName,
  _lot       :: Maybe Lot
                 }

reportToRows :: Section -> [Row]
reportToRows t =  sectionToRows ("", t)

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
  case M.toList (M.combine [subtotals, positions]) of
    [] -> [Row (Just title) Nothing Nothing Nothing Nothing]
    c ->
      getZipList $ do
        _account <- ZipList (Just title : repeat Nothing)
        _total <- Just . (!! 0) . snd <$> ZipList c
        _amount <- Just . (!! 1) . snd <$> ZipList c
        _commodity <- Just . fst . fst <$> ZipList c
        _lot <- snd . fst <$> ZipList c
        pure Row {..}


indent :: Int -> Row -> Row
indent n (Row first a b c d) = Row (indent' <$> first) a b c d
  where
    indent' t = T.replicate (fromIntegral n) " " <> t


-- formatting rows into a table
formatTable :: [Row] -> String
formatTable t =
  showTable
    [ ColDesc left "Account" left (maybe "" T.unpack . _account)
    , ColDesc left "Total" right (maybe "" formatStandard . _total)
    , ColDesc left "Amount" right (maybe "" formatStandard . _amount)
    , ColDesc left "Commodity" left (maybe "" show . _commodity)
    , ColDesc left "Lot" left (maybe "" show . _lot)
    ]
    t
    []

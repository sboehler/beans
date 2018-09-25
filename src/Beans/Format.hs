module Beans.Format
  ( Section(..)
  , formatTable
  , reportToRows
  , createHierarchicalReport
  , createFlatReport
  ) where

import           Beans.Data.Accounts       (AccountName (..), Accounts, Amount,
                                            Amounts, CommodityName (..),
                                            Lot (..))
import qualified Beans.Data.Map            as M
import           Beans.Pretty              ()
import           Beans.Table               (Column (..), formatStandard, left,
                                            right, showTable)
import qualified Data.List                 as L
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (pretty)

type Positions = M.Map (CommodityName, Maybe Lot) Amounts

data Section = Section
  { sPositions :: Positions
  , sSections  :: M.Map Text Section
  , sSubtotals :: Positions
  } deriving (Show)

-- Creating a report
createHierarchicalReport :: Accounts -> Section
createHierarchicalReport = groupSections . fmap toItem . M.toList
  where
    toItem ((a, c, l), s) = (toLabel a, M.singleton (c, l) s)
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
  { rAccount   :: Text
  , rAmount    :: Text
  , rCommodity :: Text
  }

reportToRows :: Section -> [Row]
reportToRows t = sectionToRows ("", t)

sectionToRows :: (Text, Section) -> [Row]
sectionToRows (label, Section _ subsections subtotals) =
  positionRows ++ subsectionRows
  where
    subsectionRows = indent 2 <$> (sectionToRows =<< M.toList subsections)
    positionRows = positionsToRows label subtotals

positionsToRows :: Text -> Positions -> [Row]
positionsToRows title subtotals =
  let nbrSubtotals = M.size subtotals
      nbrRows = maximum [1, nbrSubtotals]
      st =
        flattenPositions subtotals ++
        replicate (nbrRows - nbrSubtotals) (Nothing, Nothing, Nothing)
   in do (rAccount, (lot, commodity, amount)) <- zip (title : repeat "") st
         pure
           Row
             { rAccount
             , rAmount = maybe "" formatStandard amount
             , rCommodity =
                 T.pack $
                 unwords [maybe "" show commodity, maybe "" (show . pretty) lot]
             }

flattenPositions ::
     Positions -> [(Maybe Lot, Maybe CommodityName, Maybe Amount)]
flattenPositions positions = do
  (lot, amounts) <- (M.toList . M.mapKeysM snd) positions
  (commodity, amount) <- M.toList amounts
  return (lot, Just commodity, Just amount)

indent :: Int -> Row -> Row
indent n (Row first a b) = Row (indent' first) a b
  where
    indent' t = T.replicate (fromIntegral n) " " <> t

-- formatting rows into a table
formatTable :: [Row] -> Text
formatTable =
  showTable
    [ Column left "Account" left rAccount
    , Column left "Amount" right rAmount
    , Column left "Commodity" left rCommodity
    ]

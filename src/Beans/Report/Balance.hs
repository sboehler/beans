module Beans.Report.Balance
  ( Section(..)
  , formatTable
  , reportToRows
  , createReport
  )
where

import           Beans.Data.Accounts                      ( Accounts
                                                          , Amount
                                                          , Amounts
                                                          , Commodity(..)
                                                          , Lot(..)
                                                          , Position(..)
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Pretty                             ( )
import           Beans.Table                              ( Column(..)
                                                          , formatStandard
                                                          , left
                                                          , right
                                                          , showTable
                                                          )
import           Data.Foldable                            ( fold )
import           Data.Monoid                              ( (<>) )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc                ( pretty )

type Positions = M.Map (Commodity, Maybe Lot) Amounts

data Section = Section
  { sPositions :: Positions
  , sSections  :: M.Map Text Section
  , sSubtotals :: Positions
  } deriving (Show)

-- Creating a report
createReport :: (Position -> [Text]) -> Accounts -> Section
createReport label = groupLabeledPositions . M.mapEntries f
 where
  f (k@Position { pCommodity, pLot }, amount) =
    (label k, M.singleton (pCommodity, pLot) amount)

groupLabeledPositions :: M.Map [Text] Positions -> Section
groupLabeledPositions labeledPositions = Section positions
                                                 subsections
                                                 (positions <> subtotals)
 where
  positions = M.findWithDefaultM mempty labeledPositions
  subsections =
    groupLabeledPositions <$> splitSection (M.delete mempty labeledPositions)
  subtotals = fold (sSubtotals <$> subsections)

splitSection :: M.Map [Text] Positions -> M.Map Text (M.Map [Text] Positions)
splitSection = M.mapEntries f
 where
  f (n : ns, ps) = (n, M.singleton ns ps)
  f ([]    , ps) = (mempty, M.singleton [] ps)

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
  positionRows   = positionsToRows label subtotals

positionsToRows :: Text -> Positions -> [Row]
positionsToRows title subtotals =
  let
    positions = flattenPositions subtotals
    nbrRows   = maximum [1, length positions]
    quantify  = take nbrRows . (++ repeat mempty)
    accounts  = [title]
    amounts   = formatStandard . (\(_, _, amount) -> amount) <$> positions
    commodities =
      T.pack
        .   unwords
        .   (\(lot, commodity, _) ->
              [show commodity, maybe "" (show . pretty) lot]
            )
        <$> positions
  in
    zipWith3 Row (quantify accounts) (quantify amounts) (quantify commodities)

flattenPositions :: Positions -> [(Maybe Lot, Commodity, Amount)]
flattenPositions positions = do
  (lot      , amounts) <- (M.toList . M.mapKeysM snd) positions
  (commodity, amount ) <- M.toList amounts
  return (lot, commodity, amount)

indent :: Int -> Row -> Row
indent n (Row first a b) = Row (indent' first) a b
  where indent' t = T.replicate (fromIntegral n) " " <> t

-- formatting rows into a table
formatTable :: [Row] -> Text
formatTable = showTable
  [ Column left "Account"   left  rAccount
  , Column left "Amount"    right rAmount
  , Column left "Commodity" left  rCommodity
  ]

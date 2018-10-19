module Beans.Report.Balance
  ( Report(..)
  , reportToTable
  , incomeStatement
  , createReport
  , incomeStatementToTable
  )
where

import           Beans.Data.Accounts                      ( Accounts
                                                          , Account(..)
                                                          , Amount
                                                          , Amounts
                                                          , AccountType(..)
                                                          , Commodity(..)
                                                          , format
                                                          , Lot(..)
                                                          , Position(..)
                                                          , eraseLots
                                                          , summarize
                                                          )
import           Beans.Accounts                           ( calculateAccountsForDays
                                                          )
import           Beans.Ledger                             ( Ledger )
import qualified Beans.Data.Map                as M
import           Beans.Pretty                             ( pretty )
import           Beans.Table                              ( Cell(..) )
import           Beans.Options                            ( BalanceOptions(..)
                                                          , ReportType(..)
                                                          )
import           Data.Foldable                            ( fold )
import           Data.Monoid                              ( (<>) )
import           Data.Text                                ( Text )
import qualified Data.List                     as List
import qualified Beans.Ledger                  as L
import qualified Data.Text                     as T
import           Control.Monad.Catch                      ( MonadThrow )


type Positions = M.Map (Commodity, Maybe Lot) Amounts

data Report = Report
  { sPositions :: Positions
  , sReports  :: M.Map Text Report
  , sSubtotals :: Positions
  } deriving (Show)

data IncomeStatement = IncomeStatement
  { sIncome :: Report,
    sExpenses :: Report,
    sTotal :: Positions
   } deriving(Show)

-- Creating a report
createReport :: MonadThrow m => BalanceOptions -> Ledger -> m Report
createReport BalanceOptions {..} ledger = do
  [a0, a1] <- calculateAccountsForDays (L.filter balOptFilter ledger)
                                       [balOptFrom, balOptTo]
                                       mempty
  let balance =
        eraseLots balOptLots
          . summarize balOptDepth
          . M.filter (not . null)
          . fmap (M.filter (/= 0))
          $ (a1 `M.minus` a0)
  return $ accountsToReport balOptReportType balance


-- Creating a report
incomeStatement :: MonadThrow m => BalanceOptions -> Ledger -> m IncomeStatement
incomeStatement BalanceOptions {..} ledger = do
  let filtered = L.filter balOptFilter ledger
  [a0, a1] <- calculateAccountsForDays filtered [balOptFrom, balOptTo] mempty
  let balance =
        eraseLots balOptLots
          . summarize balOptDepth
          . M.filter (not . null)
          . fmap (M.filter (/= 0))
          $ (a0 `M.minus` a1)
      income = M.filterKeys ((`elem` [Income]) . aType . pAccount) balance
      expenses = M.filterKeys ((`elem` [Expenses]) . aType . pAccount) balance
      incomeSection = accountsToReport balOptReportType income
      expensesSection = accountsToReport balOptReportType expenses
      is = IncomeStatement
        incomeSection { sSubtotals = mempty }
        expensesSection { sSubtotals = mempty }
        (sSubtotals incomeSection `mappend` sSubtotals expensesSection)
  return is

incomeStatementToTable :: IncomeStatement -> [[Cell]]
incomeStatementToTable IncomeStatement {..} =
  [Separator, Separator, Separator]
    :  [AlignLeft "Account", AlignLeft "Amount", AlignLeft "Commodity"]
    :  [Separator, Separator, Separator]
    :  sectionToRows 0 ("", sIncome)
    ++ [Separator, Separator, Separator]
    :  sectionToRows 0 ("", sExpenses)
    ++ [[Separator, Separator, Separator]]
    ++ sectionToRows 0 ("Total", Report sTotal M.empty sTotal)
    ++ [[Separator, Separator, Separator]]

accountsToReport :: ReportType -> Accounts -> Report
accountsToReport reportType = groupLabeledPositions . M.mapEntries f
 where
  f (k@Position { pCommodity, pLot }, amount) =
    (labelFunction reportType k, M.singleton (pCommodity, pLot) amount)

labelFunction :: ReportType -> Position -> [Text]
labelFunction Hierarchical = T.splitOn ":" . T.pack . show . pAccount
labelFunction Flat         = pure . T.pack . show . pAccount

groupLabeledPositions :: M.Map [Text] Positions -> Report
groupLabeledPositions labeledPositions = Report positions
                                                subsections
                                                (positions <> subtotals)
 where
  positions = M.findWithDefaultM mempty labeledPositions
  subsections =
    groupLabeledPositions <$> splitReport (M.delete mempty labeledPositions)
  subtotals = fold (sSubtotals <$> subsections)

splitReport :: M.Map [Text] Positions -> M.Map Text (M.Map [Text] Positions)
splitReport = M.mapEntries f
 where
  f (n : ns, ps) = (n, M.singleton ns ps)
  f ([]    , ps) = (mempty, M.singleton [] ps)

-- Formatting a report into a table
reportToTable :: Report -> [[Cell]]
reportToTable t =
  [Separator, Separator, Separator]
    :  [AlignLeft "Account", AlignLeft "Amount", AlignLeft "Commodity"]
    :  [Separator, Separator, Separator]
    :  sectionToRows 0 ("", t)
    ++ [[Separator, Separator, Separator]]


sectionToRows :: Int -> (Text, Report) -> [[Cell]]
sectionToRows n (label, Report _ subsections subtotals) =
  positionRows ++ subsectionRows
 where
  subsectionRows = indent n <$> (sectionToRows 2 =<< M.toList subsections)
  positionRows   = positionsToRows label subtotals

positionsToRows :: Text -> Positions -> [[Cell]]
positionsToRows title subtotals =
  let
    positions = flattenPositions subtotals
    nbrRows   = maximum [if title == "" then 0 else 1, length positions]
    quantify  = take nbrRows . (++ repeat Empty)
    accounts  = [AlignLeft title]
    amounts   = AlignRight . format . (\(_, _, amount) -> amount) <$> positions
    commodities =
      AlignLeft
        .   T.pack
        .   unwords
        .   (\(lot, commodity, _) ->
              [show commodity, maybe "" (show . pretty) lot]
            )
        <$> positions
  in
    List.transpose [quantify accounts, quantify amounts, quantify commodities]

flattenPositions :: Positions -> [(Maybe Lot, Commodity, Amount)]
flattenPositions positions = do
  (lot      , amounts) <- (M.toList . M.mapKeysM snd) positions
  (commodity, amount ) <- M.toList amounts
  return (lot, commodity, amount)

indent :: Int -> [Cell] -> [Cell]
indent n (AlignLeft t   : ts) = IndentBy n t : ts
indent n (IndentBy n' t : ts) = IndentBy (n + n') t : ts
indent _ cs                   = cs

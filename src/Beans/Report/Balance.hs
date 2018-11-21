module Beans.Report.Balance
  ( Balance(..)
  , incomeStatement
  , balanceSheet
  , createBalance
  )
where

import           Prelude                           hiding ( filter )
import           Beans.Model                              ( Accounts
                                                          , Account(..)
                                                          , Amount
                                                          , Amounts
                                                          , AccountType(..)
                                                          , Commodity(..)
                                                          , Ledger
                                                          , filter
                                                          , format
                                                          , Lot(..)
                                                          , Position(..)
                                                          , eraseLots
                                                          , summarize
                                                          )
import           Beans.Accounts                           ( calculateAccountsForDays
                                                          )
import           Data.Group                               ( invert )
import qualified Beans.Data.Map                as M
import           Beans.Pretty                             ( pretty )
import           Beans.Table                              ( Cell(..)
                                                          , Table(..)
                                                          )
import           Beans.Options                            ( BalanceOptions(..)
                                                          , ReportType(..)
                                                          )
import           Data.Foldable                            ( fold )
import           Data.Monoid                              ( (<>) )
import           Data.Text                                ( Text )
import qualified Data.List                     as List
import qualified Data.Text                     as T
import           Control.Monad.Catch                      ( MonadThrow )


type Positions = M.Map (Commodity, Maybe Lot) Amounts

data Balance = Balance
  { sPositions :: Positions
  , sReports  :: M.Map Text Balance
  , sSubtotals :: Positions
  } deriving (Show)

instance Table Balance where
  toTable = balanceToTable

data IncomeStatement = IncomeStatement
  { sIncome :: Balance,
    sExpenses :: Balance,
    sTotal :: Positions
   } deriving(Show)

instance Table IncomeStatement where
  toTable = incomeStatementToTable

data BalanceSheet = BalanceSheet
  { bAssets :: Balance,
    bLiabilities :: Balance,
    bEquity :: Balance
   } deriving(Show)

instance Table BalanceSheet where
  toTable = balanceSheetToTable

-- Creating a report
createBalance :: MonadThrow m => BalanceOptions -> Ledger -> m Balance
createBalance BalanceOptions {..} ledger = do
  [a0, a1] <- calculateAccountsForDays (filter balOptFilter ledger)
                                       [balOptFrom, balOptTo]
                                       mempty
  let balance =
        eraseLots balOptLots
          . summarize balOptDepth
          . M.filter (not . null)
          . fmap (M.filter (/= 0))
          $ (a1 `M.minus` a0)
  return $ accountsToBalance balOptReportType balance


-- Creating a report
incomeStatement :: MonadThrow m => BalanceOptions -> Ledger -> m IncomeStatement
incomeStatement BalanceOptions {..} ledger = do
  let filtered = filter balOptFilter ledger
  [a0, a1] <- calculateAccountsForDays filtered [balOptFrom, balOptTo] mempty
  let balance =
        eraseLots balOptLots
          . summarize balOptDepth
          . M.filter (not . null)
          . fmap (M.filter (/= 0))
          $ (a0 `M.minus` a1)
      income = M.filterKeys ((`elem` [Income]) . aType . pAccount) balance
      expenses = M.filterKeys ((`elem` [Expenses]) . aType . pAccount) balance
      incomeSection = accountsToBalance balOptReportType income
      expensesSection = accountsToBalance balOptReportType expenses
      is = IncomeStatement
        incomeSection { sSubtotals = mempty }
        expensesSection { sSubtotals = mempty }
        (sSubtotals incomeSection `mappend` sSubtotals expensesSection)
  return is

balanceSheet :: MonadThrow m => BalanceOptions -> Ledger -> m BalanceSheet
balanceSheet BalanceOptions {..} ledger = do
  let filtered = filter balOptFilter ledger
  [a0, a1] <- calculateAccountsForDays filtered [balOptFrom, balOptTo] mempty
  let
    balance =
      eraseLots balOptLots
        . summarize balOptDepth
        . M.filter (not . null)
        . fmap (M.filter (/= 0))
        $ (a1 `M.minus` a0)
    assets = M.filterKeys ((`elem` [Assets]) . aType . pAccount) balance
    liabilities =
      invert
        <$> M.filterKeys ((`elem` [Liabilities]) . aType . pAccount) balance
    equity =
      invert <$> M.filterKeys ((`elem` [Equity]) . aType . pAccount) balance
    retainedEarnings =
      invert
        <$> M.filterKeys ((`elem` [Income, Expenses]) . aType . pAccount)
                         balance
    retEarn = M.mapKeysM
      (\p -> p { pAccount = Account Equity ["RetainedEarnings"] })
      retainedEarnings

    assetsSection      = accountsToBalance balOptReportType assets
    liabilitiesSection = accountsToBalance balOptReportType liabilities
    equitySection      = accountsToBalance balOptReportType (equity <> retEarn)
  return $ BalanceSheet assetsSection liabilitiesSection equitySection

incomeStatementToTable :: IncomeStatement -> [[Cell]]
incomeStatementToTable IncomeStatement {..} =
  [Separator, Separator, Separator]
    :  [AlignLeft "Account", AlignLeft "Amount", AlignLeft "Commodity"]
    :  [Separator, Separator, Separator]
    :  sectionToRows 0 ("", sIncome)
    ++ [Separator, Separator, Separator]
    :  sectionToRows 0 ("", sExpenses)
    ++ [[Separator, Separator, Separator]]
    ++ sectionToRows 0 ("Total", Balance sTotal M.empty sTotal)
    ++ [[Separator, Separator, Separator]]

balanceSheetToTable :: BalanceSheet -> [[Cell]]
balanceSheetToTable BalanceSheet {..} =
  let
    header  = AlignLeft <$> ["Account", "Amount", "Commodity"]
    sep     = replicate 3 Separator
    header' = sep : header : pure sep
    filler  = repeat $ replicate 3 Empty
    aSide   = header' ++ sectionToRows 0 ("", bAssets { sSubtotals = mempty })
    leSide =
      header'
        ++ sectionToRows 0 ("", bLiabilities { sSubtotals = mempty })
        ++ sectionToRows 0 ("", bEquity { sSubtotals = mempty })
    totalAssets =
      sectionToRows 0 ("Total", Balance mempty M.empty (sSubtotals bAssets))
    totalLiabilitiesAndEquity = sectionToRows
      0
      ( "Total"
      , Balance mempty M.empty (sSubtotals bLiabilities <> sSubtotals bEquity)
      )
    nbrRows   = maximum [length aSide, length leSide]
    nbrTotals = maximum [length totalAssets, length totalLiabilitiesAndEquity]
    aSide'    = take nbrRows (aSide ++ filler) ++ [sep] ++ take
      nbrTotals
      (totalAssets ++ filler)
    leSide' = take nbrRows (leSide ++ filler) ++ [sep] ++ take
      nbrTotals
      (totalLiabilitiesAndEquity ++ filler)
  in
    zipWith (++) aSide' leSide' ++ [replicate 6 Separator]




accountsToBalance :: ReportType -> Accounts -> Balance
accountsToBalance reportType = groupLabeledPositions . M.mapEntries f
 where
  f (k@Position { pCommodity, pLot }, amount) =
    (labelFunction reportType k, M.singleton (pCommodity, pLot) amount)

labelFunction :: ReportType -> Position -> [Text]
labelFunction Hierarchical = T.splitOn ":" . T.pack . show . pAccount
labelFunction Flat =
  (\(Account t a) -> [T.pack $ show t, T.intercalate ":" a]) . pAccount

groupLabeledPositions :: M.Map [Text] Positions -> Balance
groupLabeledPositions labeledPositions = Balance positions
                                                 subsections
                                                 (positions <> subtotals)
 where
  positions = M.findWithDefaultM mempty labeledPositions
  subsections =
    groupLabeledPositions <$> splitBalance (M.delete mempty labeledPositions)
  subtotals = fold (sSubtotals <$> subsections)

splitBalance :: M.Map [Text] Positions -> M.Map Text (M.Map [Text] Positions)
splitBalance = M.mapEntries f
 where
  f (n : ns, ps) = (n, M.singleton ns ps)
  f ([]    , ps) = (mempty, M.singleton [] ps)

-- Formatting a report into a table
balanceToTable :: Balance -> [[Cell]]
balanceToTable t =
  [Separator, Separator, Separator]
    :  [AlignLeft "Account", AlignLeft "Amount", AlignLeft "Commodity"]
    :  [Separator, Separator, Separator]
    :  sectionToRows 0 ("", t)
    ++ [[Separator, Separator, Separator]]


sectionToRows :: Int -> (Text, Balance) -> [[Cell]]
sectionToRows n (label, Balance _ subsections subtotals) =
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

module Beans.Report.Balance
  ( BalanceReport(..)
  , incomeStatement
  , balanceSheet
  , createBalanceReport
  )
where

import           Beans.Accounts                 ( sumUntil )
import qualified Beans.Data.Map                as M
import           Beans.Model
import           Beans.Options                  ( BalanceOptions(..)
                                                , ReportType(..)
                                                )
import qualified Beans.Pretty                  as P
import qualified Beans.Table                   as TB
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Foldable                  ( fold )
import           Data.Group                     ( invert )
import qualified Data.List                     as List
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as PT
import           Control.Lens
import           Prelude                 hiding ( filter )

data BalanceReport = BalanceReport
  { _balancePositions :: Accounts
  , _balanceReports  :: M.Map Text BalanceReport
  , _balanceSubtotals :: Accounts
  } deriving (Show)

instance TB.Table BalanceReport where
  toTable t = [sep, header, sep] ++ sectionToRows 0 "" t ++ [sep]
   where
    sep    = [TB.Separator, TB.Separator, TB.Separator]
    header = TB.AlignLeft <$> ["Account", "Amount", "Commodity"]

data IncomeStatement = IncomeStatement
  { sIncome :: BalanceReport,
    sExpenses :: BalanceReport,
    sTotal :: Accounts
   } deriving(Show)

instance TB.Table IncomeStatement where
  toTable IncomeStatement {..} =
    [sep, header, sep]
      ++ sectionToRows 0 "" sIncome
      ++ [sep]
      ++ sectionToRows 0 "" sExpenses
      ++ [sep]
      ++ sectionToRows 0 "Total" (BalanceReport sTotal M.empty sTotal)
      ++ [sep]
   where
    sep    = replicate 3 TB.Separator
    header = TB.AlignLeft <$> ["Account", "Amount", "Commodity"]


data BalanceReportSheet = BalanceReportSheet
  { bAssets :: BalanceReport,
    bLiabilities :: BalanceReport,
    bEquity :: BalanceReport,
    bTotalAssets :: Accounts,
    bTotalLiabilityAndEquity :: Accounts
   } deriving(Show)

instance TB.Table BalanceReportSheet where
  toTable BalanceReportSheet {..} =
    [sep, header, sep]
      ++ List.transpose (align $ lCols ++ rCols)
      ++ [sep]
      ++ List.transpose (align $ lColsTotal ++ rColsTotal)
      ++ [sep]
   where
    sep        = replicate 6 TB.Separator
    header     = take 6 $ cycle h
    h          = TB.AlignLeft <$> ["Account", "Amount", "Commodity"]
    lCols      = makeCols "" bAssets
    rCols      = zipWith (++) (makeCols "" bLiabilities) (makeCols "" bEquity)
    lColsTotal = makeCols "Total" $ BalanceReport mempty M.empty bTotalAssets
    rColsTotal =
      makeCols "Total" $ BalanceReport mempty M.empty bTotalLiabilityAndEquity
    makeCols l r = List.transpose $ sectionToRows 0 l r


ledgerToAccounts :: MonadThrow m => BalanceOptions -> Ledger -> m Accounts
ledgerToAccounts BalanceOptions {..} ledger = do
  (a0, l1) <- sumUntil balOptFrom (filter balOptFilter ledger) mempty
  (a1, _ ) <- sumUntil balOptTo l1 a0
  return
    . eraseLots balOptLots
    . summarize balOptDepth
    . M.filter (not . null)
    . fmap (M.filter (/= 0))
    $ (a1 `M.minus` a0)

ofType :: [AccountType] -> Position -> Bool
ofType types = view $ account . accountType . to (`elem` types)


-- Creating a report
createBalanceReport
  :: MonadThrow m => BalanceOptions -> Ledger -> m BalanceReport
createBalanceReport options ledger = do
  bal <- ledgerToAccounts options ledger
  return $ balanceReport (balOptReportType options) bal

-- Creating a report
incomeStatement :: MonadThrow m => BalanceOptions -> Ledger -> m IncomeStatement
incomeStatement options ledger = do
  b <- ledgerToAccounts options ledger
  let income   = toReport $ invert $ M.filterKeys (ofType [Income]) b
      expenses = toReport $ invert $ M.filterKeys (ofType [Expenses]) b
  return IncomeStatement
    { sIncome   = income { _balanceSubtotals = mempty }
    , sExpenses = expenses { _balanceSubtotals = mempty }
    , sTotal    = _balanceSubtotals income <> _balanceSubtotals expenses
    }
  where toReport = balanceReport (balOptReportType options)


balanceSheet :: MonadThrow m => BalanceOptions -> Ledger -> m BalanceReportSheet
balanceSheet options ledger = do
  b <- ledgerToAccounts options ledger
  let (assets     , rest  ) = M.partitionKeys (ofType [Assets]) b
      (liabilities, equity) = M.partitionKeys (ofType [Liabilities]) rest
      a                     = convert assets
      l                     = convert $ invert liabilities
      e                     = convert $ invert $ M.mapKeysM mapRE equity
  return $ BalanceReportSheet
    { bAssets                  = a { _balanceSubtotals = mempty }
    , bLiabilities             = l { _balanceSubtotals = mempty }
    , bEquity                  = e { _balanceSubtotals = mempty }
    , bTotalAssets             = _balanceSubtotals a
    , bTotalLiabilityAndEquity = _balanceSubtotals e <> _balanceSubtotals l
    }
 where
  convert = balanceReport (balOptReportType options)
  mapRE pos = if pos ^. account . accountType == Equity
    then pos
    else pos & account .~ Account Equity ["RetainedEarnings"]


balanceReport :: ReportType -> Accounts -> BalanceReport
balanceReport reportType = groupLabeledPositions . M.mapEntries f
  where f (pos, amts) = (labelFunction reportType pos, M.singleton pos amts)

labelFunction :: ReportType -> Position -> [Text]
labelFunction Hierarchical = T.splitOn ":" . T.pack . show . _positionAccount
labelFunction Flat =
  (\(Account t a) -> [T.pack $ show t, T.intercalate ":" a]) . _positionAccount

groupLabeledPositions :: M.Map [Text] Accounts -> BalanceReport
groupLabeledPositions labeledPositions = BalanceReport
  positions
  subsections
  (positions <> subtotals)
 where
  positions   = M.findWithDefaultM [] labeledPositions
  subsections = groupLabeledPositions
    <$> splitBalanceReport (M.delete mempty labeledPositions)
  subtotals = fold (_balanceSubtotals <$> subsections)

splitBalanceReport
  :: M.Map [Text] Accounts -> M.Map Text (M.Map [Text] Accounts)
splitBalanceReport = M.mapEntries f
 where
  f (n : ns, ps) = (n, M.singleton ns ps)
  f ([]    , ps) = (mempty, M.singleton [] ps)

sectionToRows :: Int -> Text -> BalanceReport -> [[TB.Cell]]
sectionToRows n label (BalanceReport _ subsections subtotals) =
  positionRows ++ subsectionRows
 where
  subsectionRows = do
    (l, report) <- M.toList subsections
    indent n <$> sectionToRows 2 l report
  positionRows = positionsToRows label subtotals

positionsToRows :: Text -> Accounts -> [[TB.Cell]]
positionsToRows title subtotals = List.transpose $ align columns
 where
  positions    = flattenPositions subtotals
  colAccount   = [TB.AlignLeft title]
  colAmount    = TB.AlignRight . format . snd <$> positions
  colCommodity = TB.AlignLeft <$> do
    (l, c) <- fst <$> positions
    let doc = P.pretty c P.<+> maybe P.emptyDoc P.pretty l
    return $ PT.renderStrict $ P.layoutCompact doc
  columns = [colAccount, colAmount, colCommodity]


align :: [[TB.Cell]] -> [[TB.Cell]]
align columns = do
  col <- columns
  return $ take l $ col ++ repeat TB.Empty
  where l = maximum $ length <$> columns

flattenPositions :: Accounts -> [((Maybe Lot, Commodity), Amount)]
flattenPositions positions = do
  (l, amounts) <- M.toList $ M.mapKeysM (view lot) positions
  (c, a      ) <- M.toList amounts
  return ((l, c), a)

indent :: Int -> [TB.Cell] -> [TB.Cell]
indent n (TB.AlignLeft t   : ts) = TB.IndentBy n t : ts
indent n (TB.IndentBy n' t : ts) = TB.IndentBy (n + n') t : ts
indent _ cs                      = cs

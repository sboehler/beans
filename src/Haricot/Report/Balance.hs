module Haricot.Report.Balance where

import           Data.Bifunctor           (second)
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Data.Text.Lazy           (Text, pack)
import           Haricot.Accounts         (Accounts, Key (..))
import           Haricot.AST              (AccountName (..), Lot (NoLot))
import           Haricot.Report.Table     (ColDesc (..), left, right, showTable)

data Entry = Entry
  { _labels :: [Text]
  , _entry  :: (Key, Scientific)
  } deriving (Eq, Ord)

data Report
  = Group Text
          [Report]
  | Single Entry

createReports :: [Entry] -> [Report]
createReports entries =
  let grouped = entries `groupBy` classify
      reports = createReport <$> grouped
   in concat reports

createReport :: (Maybe Text, [Entry]) -> [Report]
createReport (Just n, es)  = [Group n (createReports es)]
createReport (Nothing, es) = Single <$> es

classify :: Entry -> (Maybe Text, Entry)
classify (Entry (s:ss) e) = (Just s, Entry ss e)
classify (Entry [] e)     = (Nothing, Entry [] e)

groupBy :: (Ord k) => [a] -> (a -> (k, v)) -> [(k, [v])]
groupBy list key = M.toList $ M.fromListWith (++) (second pure . key <$> list)

printAccounts :: Accounts -> IO ()
printAccounts accounts =
  putStrLn $
  showTable
    [ ColDesc left "Account" left (show . keyAccount . fst)
    , ColDesc left "Amount" right (format . snd)
    , ColDesc left "Commodity" left (show . keyCommodity . fst)
    , ColDesc left "Lot" left (show . keyLot . fst)
    ]
    items
    totals
  where
    format = formatStandard
    items = M.toList accounts
    totals = M.toList . eraseLots . eraseAccounts "Total" $ accounts

formatStandard :: Scientific -> String
formatStandard = formatScientific Fixed (Just 2)

-- formatK :: Scientific -> String
-- formatK n = formatScientific Fixed (Just 0) (n `sdiv` 1000) ++ "k"

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysWith (+) m
  where
    m Key {..} = Key {keyAccount = AccountName $ take d (_unAccountName keyAccount), ..}

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysWith (+) (\k -> k {keyLot = NoLot})

eraseAccounts :: Text -> Accounts -> Accounts
eraseAccounts label = M.mapKeysWith (+) m
  where
    m k = k { keyAccount = AccountName [label, pack $ show (keyCommodity k)] }

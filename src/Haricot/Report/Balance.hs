module Haricot.Report.Balance where

import           Data.List                as L
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Data.Text.Lazy           (Text, pack)
import           Haricot.Accounts         (Accounts, Key (..))
import           Haricot.AST              (AccountName (..), 
                                           Lot (NoLot))
import           Haricot.Report.Table     (ColDesc (..), left, right, showTable)

type Entry = (Key, Scientific)


data Item
  = EntryItem Entry
  | GroupItem Group

data Group = Group
  { _name   :: AccountName
  , _items  :: [Item]
  , _totals :: Accounts
  }

tree :: Group -> Group
tree a = a

splitAccountName :: Int -> AccountName -> (AccountName, AccountName)
splitAccountName n (AccountName a) =
  let (prefix, suffix) = L.splitAt n a
   in (AccountName prefix, AccountName suffix)

part :: Ord l => (a -> l) -> [a] -> [(l, [a])]
part key list =
  let k = zip (key <$> list) (pure <$> list)
   in M.toList $ M.fromListWith (++) k

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

module Haricot.Report.Balance where

import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Data.Text.Lazy           (Text, pack)
import           Haricot.Accounts         (Accounts)
import           Haricot.AST              (AccountName (..), CommodityName,
                                           Lot (NoLot))
import           Haricot.Report.Table     (ColDesc (..), left, right, showTable)

type Item = ((AccountName, CommodityName, Lot), Scientific)

data Group
  = Group Text [Item]
  | Groups [Group]

printAccounts :: Accounts -> IO ()
printAccounts accounts =
  putStrLn $
  showTable
    [ ColDesc left "Account" left (show . (\(a, _, _) -> a) . fst)
    , ColDesc left "Amount" right (format . snd)
    , ColDesc left "Commodity" left (show . (\(_, c, _) -> c) . fst)
    , ColDesc left "Lot" left (show . (\(_, _, l) -> l) . fst)
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
    m (AccountName n, c, l) = (AccountName $ take d n, c, l)

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysWith (+) (\(a, c, _) -> (a, c, NoLot))

eraseAccounts :: Text -> Accounts -> Accounts
eraseAccounts label = M.mapKeysWith (+) m
  where
    m (_, c, l) = (AccountName [label, pack $ show c], c, l)

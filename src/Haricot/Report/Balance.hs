module Haricot.Report.Balance
  ( printAccounts
  , summarize
  , eraseLots
  ) where

import qualified Data.Map.Strict.Extended as M
import           Data.Scientific          (FPFormat (Fixed), formatScientific)
import           Data.Text.Lazy           (Text, pack)
import           Haricot.Accounts         (Accounts)
import           Haricot.AST              (AccountName (..), Lot (NoLot))
import           Haricot.Report.Table     (ColDesc (..), left, right, showTable)

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
    format = formatScientific Fixed (Just 2)
    items = M.toList accounts
    totals = M.toList $ eraseLots $ eraseAccounts "Total" accounts

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

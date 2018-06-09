module Beans.Report.Balance where

import           Beans.Accounts           (Accounts, Key (..))
import           Beans.AST                (AccountName (..))
import           Beans.Report.Table       (ColDesc (..), left, right, showTable)
import qualified Data.Map.Strict.Extended as M
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)

printAccounts :: Accounts -> IO ()
printAccounts accounts =
  putStrLn $
  showTable
    [ ColDesc left "Account" left (maybe "" show . keyAccount . fst)
    , ColDesc left "Amount" right (format . snd)
    , ColDesc left "Commodity" left (show . keyCommodity . fst)
    , ColDesc left "Lot" left (maybe "" show . keyLot . fst)
    ]
    items
    totals
  where
    format = formatStandard
    items = M.toList accounts
    totals = M.toList . eraseLots . eraseAccounts $ accounts

formatStandard :: Scientific -> String
formatStandard = formatScientific Fixed (Just 2)

-- formatK :: Scientific -> String
-- formatK n = formatScientific Fixed (Just 0) (n `sdiv` 1000) ++ "k"

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysWith (+) m
  where
    m Key {..} = Key {keyAccount = shorten d <$> keyAccount, ..}

shorten :: Int -> AccountName -> AccountName
shorten d a = a {_unAccountName = take d (_unAccountName a)}

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysWith (+) (\k -> k {keyLot = Nothing})

eraseAccounts ::  Accounts -> Accounts
eraseAccounts = M.mapKeysWith (+) (\k -> k {keyAccount = Nothing})

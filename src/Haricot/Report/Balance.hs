module Haricot.Report.Balance
  ( printAccounts
  ) where

import           Data.Scientific      (FPFormat (Fixed), Scientific,
                                       formatScientific)
import           Haricot.Accounts     (Accounts, mapWithKeys)
import           Haricot.AST          (AccountName, CommodityName, Lot)
import           Haricot.Report.Table (ColDesc (..), left, right, showTable)


data Row = Row
  { account   :: AccountName
  , commodity :: CommodityName
  , lot       :: Lot
  , amount    :: Scientific
  } deriving (Show)

printAccounts :: Accounts -> IO()
printAccounts accounts =
  putStrLn $
  showTable
    [ ColDesc left "Account" left (show . account)
    , ColDesc left "Amount" right (formatScientific Fixed (Just 2) . amount)
    , ColDesc left "Commodity" left (show . commodity)
    , ColDesc left "Lot" left (show . lot)
    ]
    (mapWithKeys Row accounts)

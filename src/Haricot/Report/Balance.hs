module Haricot.Report.Balance
  ( printAccounts
  , summarize
  , eraseLots
  ) where

import qualified Data.Map             as M
import           Data.Scientific      (FPFormat (Fixed), Scientific,
                                       formatScientific)
import           Haricot.Accounts     (Accounts, mapWithKeys, Account(..))
import           Haricot.AST          (AccountName(..), CommodityName, Lot(NoLot))
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
    [ ColDesc left "Account" left (show . account) (const "Total")
    , ColDesc left "Amount" right (format . amount) (format . sum . map amount)
    , ColDesc left "Commodity" left (show . commodity) (const "")
    , ColDesc left "Lot" left (show . lot) (const "")
    ]
    (mapWithKeys Row accounts)
  where
    format = formatScientific Fixed (Just 2) 

summarize :: Int -> Accounts -> Accounts
summarize depth  = M.mapKeysWith mappend m
  where
    m (AccountName l) = AccountName $ take depth l

eraseLots :: Account -> Account
eraseLots Account {_holdings, ..} =
  Account {_holdings = M.mapKeysWith (+) (const NoLot) <$> _holdings, ..}

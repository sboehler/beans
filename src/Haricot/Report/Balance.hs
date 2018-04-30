module Haricot.Report.Balance
  ( printAccounts
  , summarize
  , combine
  ) where

import qualified Data.Map             as M
import           Data.Scientific      (FPFormat (Fixed), Scientific,
                                       formatScientific)
import           Haricot.Accounts     (Accounts, mapWithKeys, Holdings)
import           Haricot.AST          (AccountName(..), CommodityName, Lot)
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
    , ColDesc left "Lot" left (show . lot)
    , ColDesc left "Amount" right (formatScientific Fixed (Just 2) . amount)
    , ColDesc left "Commodity" left (show . commodity)
    ]
    (mapWithKeys Row accounts)

summarize :: Int -> Accounts -> Accounts
summarize depth  = M.mapKeysWith mappend m
  where
    m (AccountName l) = AccountName $ take depth l

combine :: Holdings -> Holdings -> M.Map CommodityName Scientific
h1 `combine` h2 = M.unionWith (+) (sum <$> h1) (sum <$> h2)

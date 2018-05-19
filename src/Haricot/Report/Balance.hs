module Haricot.Report.Balance
  ( printAccounts
  , summarize
  , eraseLots
  ) where

import qualified Data.Map.Strict.Extended as M
import           Data.Scientific          (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Haricot.Accounts         (Accounts)
import           Haricot.AST              (AccountName (..), CommodityName, Lot(NoLot))
import           Haricot.Report.Table     (ColDesc (..), left, right, showTable)

data Row = Row
  { account   :: AccountName
  , commodity :: CommodityName
  , lot       :: Lot
  , amount    :: Scientific
  } deriving (Show)

printAccounts :: Accounts -> IO ()
printAccounts accounts =
  putStrLn $
  showTable
    [ ColDesc left "Account" left (show . account) (const "Total")
    , ColDesc left "Amount" right (format . amount) (format . sum . map amount)
    , ColDesc left "Commodity" left (show . commodity) (const "")
    , ColDesc left "Lot" left (show . lot) (const "")
    ]
    (M.toListWith (\((a, c, l), n) -> Row a c l n) accounts)
  where
    format = formatScientific Fixed (Just 2)

summarize :: Int -> Accounts -> Accounts
summarize d = M.mapKeysWith (+) m
  where
    m (AccountName n, c, l) = (AccountName $ take d n, c, l)

eraseLots :: Accounts -> Accounts
eraseLots = M.mapKeysWith (+) (\(a, c, _) -> (a, c, NoLot))

module Data.Posting
  ( PostingPrice(..)
  , Posting(..)
  ) where

import Data.AccountName (AccountName)
import Data.Amount (Amount)
import Data.Commodity (CommodityName)
import Data.Lot (Lot)
import Data.Price (Price)

data Posting a = Posting
  { _accountName :: AccountName
  , _amount :: a
  , _commodity :: CommodityName
  , _price :: Maybe (PostingPrice a)
  , _lot :: Maybe (Lot a)
  } deriving (Show, Eq)

data PostingPrice a
  = UnitPrice (Price a)
  | TotalPrice (Amount a)
  deriving (Show, Eq)

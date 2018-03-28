module Data.Posting
  ( PostingPrice(..)
  , Posting(..)
  ) where

import Data.AccountName (AccountName)
import Data.Amount      (Amount)
import Data.Commodity   (CommodityName)
import Data.Lot         (Lot)
import Data.Scientific(Scientific)

data Posting = Posting
  { _accountName :: AccountName
  , _amount ::Scientific 
  , _commodity :: CommodityName
  , _price :: Maybe PostingPrice
  , _lot :: Maybe Lot
  } deriving (Show, Eq, Ord)

data PostingDirective
  = WildcardPosting AccountName
  | CompletePosting (Posting )
  deriving (Show, Eq, Ord)

data PostingPrice 
  = UnitPrice Amount
  | TotalPrice Amount
  deriving (Show, Eq, Ord)

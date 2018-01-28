module Data.Account
  ( Account(..)
  , addPosting
  , AccountName(..)
  ) where

import Data.AccountName (AccountName(..))
import qualified Data.Accounts as A
import Data.Commodity (CommodityName)
import qualified Data.Holdings as H
import Data.Posting (Posting(..))

data Account a = Account
  { _accounts :: A.Accounts (Account a)
  , _holdings :: H.Holdings a
  } deriving (Show, Eq)

instance Num a => Monoid (Account a) where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

addPosting :: Num a => Posting a -> Account a -> Account a
addPosting p@Posting {..} (Account a h) =
  case _accountName of
    (AccountName (n:ns)) ->
      let p' = p {_accountName = AccountName ns}
          a' = A.adjust (addPosting p') n a
      in Account a' h
    (AccountName []) ->
      let h' = H.insert h _commodity _amount
      in Account a h'

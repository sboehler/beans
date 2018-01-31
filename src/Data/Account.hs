module Data.Account
  ( Account(..)
  , insert
  , AccountName(..)
  ) where

import Control.Lens ((%~), makeLenses)
import Data.AccountName (AccountName(..))
import qualified Data.Accounts as A
import qualified Data.Holdings as H
import Data.Posting (Posting(..))

data Account a = Account
  { _accounts :: A.Accounts (Account a)
  , _holdings :: H.Holdings a
  } deriving (Show, Eq)

instance (Ord a, Num a) => Monoid (Account a) where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

makeLenses 'Account

insert :: (Ord a, Num a) => Posting a -> Account a -> Account a
insert p =
  case _unAccountName (_accountName p) of
    (n:ns) ->
      accounts %~
      A.adjustWithDefault (insert p {_accountName = AccountName ns}) n
    [] -> holdings %~ H.insert p

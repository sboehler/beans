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

data Account = Account
  { _accounts :: A.Accounts Account
  , _holdings :: H.Holdings 
  } deriving (Show, Eq)

instance Monoid Account  where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

makeLenses 'Account

insert ::  Posting  -> Account  -> Account 
insert p =
  case _unAccountName (_accountName p) of
    (n:ns) ->
      accounts %~
      A.adjustWithDefault (insert p {_accountName = AccountName ns}) n
    [] -> holdings %~ H.insert p

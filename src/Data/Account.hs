module Data.Account
  ( Account(..)
  , addPosting
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

instance Num a => Monoid (Account a) where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

makeLenses 'Account

addPosting :: Num a => Posting a -> Account a -> Account a
addPosting Posting {..} =
  case _unAccountName _accountName of
    (n:ns) ->
      let f = addPosting Posting {_accountName = AccountName ns, ..}
      in accounts %~ A.adjust f n
    [] -> holdings %~ H.insert _commodity _amount

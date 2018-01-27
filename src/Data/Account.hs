module Data.Account
  ( Account(..)
  , Accounts(..)
  , insert
  , addPosting
  , AccountName(..)
  ) where

import Data.AccountName (AccountName(..))
import Data.Amount (Amount(..))
import qualified Data.Holdings as H
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..))
import Data.Text.Lazy (Text)

newtype Accounts a = Accounts
  { _unAccounts :: M.Map Text (Account a)
  } deriving (Show, Eq)

instance Num a => Monoid (Accounts a) where
  mempty = Accounts mempty
  mappend (Accounts a) (Accounts a') = Accounts $ M.unionWith mappend a a'

data Account a = Account
  { _accounts :: Accounts a
  , _holdings :: H.Holdings a
  } deriving (Show, Eq)

instance Num a => Monoid (Account a) where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

insert :: Num a => Account a -> Amount a -> Account a
insert (Account accounts h) a = Account accounts (H.insert h a)

addPosting :: Num a => Account a -> Posting a -> Account a
addPosting account Posting {..} = f (_unAccountName _accountName) account
  where
    f (n:ns) (Account (Accounts accounts) h) =
      Account (Accounts $ M.insert n account' accounts) h
      where
        account' = f ns (M.findWithDefault mempty n accounts)
    f [] (Account a h) = Account a (H.insert h _amount)

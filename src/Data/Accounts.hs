module Data.Accounts
  ( Accounts(..)
  , find
  , insert
  , adjust
  ) where

import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text)

newtype Accounts a = Accounts
  { _unAccounts :: M.Map Text a
  } deriving (Show, Eq)

instance Monoid a => Monoid (Accounts a) where
  mempty = Accounts mempty
  (Accounts a) `mappend` (Accounts a') = Accounts $ M.unionWith mappend a a'

find :: (Monoid a) => Text -> Accounts a -> a
find n = M.findWithDefault mempty n . _unAccounts

insert :: Text -> a -> Accounts a -> Accounts a
insert n x (Accounts m) = Accounts $ M.insert n x m

adjust :: (Monoid a) => (a -> a) -> Text -> Accounts a -> Accounts a
adjust f n (Accounts m) = Accounts $ M.alter g n m
  where
    g Nothing = Just $ f mempty
    g (Just a) = Just $ f a

module Data.Account
  ( Account(..)
  , AccountName(..)
  ) where

import Data.Holdings (Holdings(..))
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text, intercalate, unpack)
import Data.Text.Prettyprint.Doc (Pretty, (<+>), pretty, vsep)

newtype AccountName = AccountName
  { _unAccountName :: [Text]
  } deriving (Eq)

instance Show AccountName where
  show = unpack . intercalate ":" . _unAccountName

instance Pretty AccountName where
  pretty = pretty . show

newtype Accounts a = Accounts
  { _unAccounts :: M.Map Text (Account a)
  } deriving (Show, Eq)

instance (Show a) => Pretty (Accounts a) where
  pretty (Accounts m) = vsep (map f (M.toList m))
    where
      f (k, v) = pretty k <+> pretty v

instance Num a => Monoid (Accounts a) where
  mempty = Accounts mempty
  mappend (Accounts a) (Accounts a') = Accounts $ M.unionWith mappend a a'

data Account a = Account
  { _accounts :: Accounts a
  , _holdings :: Holdings a
  } deriving (Show, Eq)

instance (Show a) => Pretty (Account a) where
  pretty (Account a h) = pretty a <+> pretty h

instance Num a => Monoid (Account a) where
  mempty = Account mempty mempty
  (Account a h) `mappend` (Account a' h') =
    Account (a `mappend` a') (h `mappend` h')

module Data.Account
  ( Accounts(..)
  , AccountName(..)
  ) where

import Data.Holdings (Holdings)
import qualified Data.Map.Lazy as M
import Data.Text.Lazy (Text, intercalate, unpack)
import Data.Text.Prettyprint.Doc

newtype Accounts a = Accounts
  { _unAccounts :: M.Map AccountName (Holdings a)
  }

newtype AccountName = AccountName
  { _unAccountName :: [Text]
  } deriving (Eq)

instance Show AccountName where
  show = unpack . intercalate ":" . _unAccountName

instance Pretty AccountName where
  pretty = pretty . show
{- fromPostings :: [Posting] -> Accounts -}
{- fromPostings =  -}

module Data.AccountName
  ( AccountName(..)
  ) where

import Data.Text.Lazy (Text, intercalate, unpack)

newtype AccountName = AccountName
  { _unAccountName :: [Text]
  } deriving (Eq, Ord)

instance Show AccountName where
  show = unpack . intercalate ":" . _unAccountName

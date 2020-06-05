module Beans.Filter
  ( Filter (..),
    AccountFilter (AccountFilter),
    CommodityFilter (CommodityFilter),
  )
where

data Filter = Filter AccountFilter CommodityFilter
  deriving (Show)

newtype AccountFilter = AccountFilter String
  deriving (Show)

newtype CommodityFilter = CommodityFilter String
  deriving (Show)

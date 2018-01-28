module Data.Holdings
  ( Holdings(..)
  , toList
  , fromList
  , insert
  , filter
  ) where

import Data.Commodity (CommodityName)
import qualified Data.Map.Lazy as M
import Prelude hiding (filter)

newtype Holdings a = Holdings
  { _unHoldings :: M.Map CommodityName a
  } deriving (Show, Eq, Functor)

instance Num a => Monoid (Holdings a) where
  mempty = Holdings M.empty
  (Holdings a) `mappend` (Holdings b) = Holdings (M.unionWith (+) a b)

toList :: Holdings a -> [(CommodityName, a)]
toList = M.toList . _unHoldings

fromList :: Num a => [(CommodityName, a)] -> Holdings a
fromList = Holdings . M.fromListWith (+)

filter :: (a -> Bool) -> Holdings a -> Holdings a
filter f h = Holdings $ M.filter f (_unHoldings h)

insert :: Num a => CommodityName -> a -> Holdings a -> Holdings a
insert c a (Holdings m) = Holdings $ M.insertWith (+) c a m

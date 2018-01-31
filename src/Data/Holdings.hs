module Data.Holdings
  ( Holdings(..)
  , insert
  ) where

import Data.Commodity (CommodityName)
import qualified Data.Lots as L
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..))
import Data.Util (adjustWithDefault)

newtype Holdings a = Holdings
  { _unHoldings :: M.Map CommodityName (L.Lots a)
  } deriving (Show, Eq)

instance (Ord a, Num a) => Monoid (Holdings a) where
  mempty = Holdings M.empty
  (Holdings a) `mappend` (Holdings b) = Holdings (M.unionWith mappend a b)

insert :: (Ord a, Num a) => Posting a -> Holdings a -> Holdings a
insert p (Holdings m) =
  Holdings $ adjustWithDefault (L.insert p) (_commodity p) m

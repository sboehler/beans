module Data.Holdings
  ( Holdings(..)
  , insert
  ) where

import Data.Commodity (CommodityName)
import qualified Data.Lots as L
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..))
import Data.Util (adjustWithDefault)

newtype Holdings = Holdings
  { _unHoldings :: M.Map CommodityName (L.Lots)
  } deriving (Show, Eq)

instance  Monoid (Holdings ) where
  mempty = Holdings M.empty
  (Holdings a) `mappend` (Holdings b) = Holdings (M.unionWith mappend a b)

insert :: Posting  -> Holdings  -> Holdings 
insert p (Holdings m) =
  Holdings $ adjustWithDefault (L.insert p) (_commodity p) m

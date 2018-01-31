module Data.Lots
  ( Lots(..)
  , insert
  ) where

import Data.Lot (Lot)
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..))

newtype Lots a = Lots
  { _unLots :: M.Map (Maybe (Lot a)) a
  } deriving (Show, Eq)

instance (Ord a, Num a) => Monoid (Lots a) where
  mempty = Lots M.empty
  (Lots a) `mappend` (Lots b) = Lots (M.unionWith (+) a b)

insert :: (Ord a, Num a) => Posting a -> Lots a -> Lots a
insert Posting {_lot, _amount} (Lots lots) =
  Lots $ M.insertWith (+) _lot _amount lots

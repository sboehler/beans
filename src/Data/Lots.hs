module Data.Lots
  ( Lots(..)
  , insert
  ) where

import Data.Lot (Lot)
import qualified Data.Map.Lazy as M
import Data.Posting (Posting(..))
import Data.Scientific (Scientific)

newtype Lots = Lots
  { _unLots :: M.Map (Maybe Lot) Scientific
  } deriving (Show, Eq)

instance Monoid Lots  where
  mempty = Lots M.empty
  (Lots a) `mappend` (Lots b) = Lots (M.unionWith (+) a b)

insert :: Posting -> Lots -> Lots 
insert Posting {_lot, _amount} (Lots lots) =
  Lots $ M.insertWith (+) _lot _amount lots

module Beans.Data.Map
  ( Map
  , insert
  , mapKeys
  , filter
  , filterByKey
  , toList
  , split
  ) where

import Prelude hiding (filter)
import           Data.Foldable    (Foldable)
import           Data.Group       (Group (..))
import qualified Data.Map.Strict  as M
import           Data.Traversable (Traversable (..))

newtype Map k v = Map {
  _unmap :: M.Map k v
}

instance (Monoid v, Ord k) => Monoid (Map k v) where
  mempty = Map mempty
  (Map m1) `mappend` (Map m2) = Map (M.unionWith mappend m1 m2)

instance (Group v, Ord k) => Group (Map k v) where
  invert (Map v) = Map (invert <$> v)

instance Functor (Map k) where
  fmap f (Map m) = Map $ fmap f m

instance Foldable (Map k) where
  foldMap f (Map m) = foldMap f m

instance Traversable (Map k) where
  traverse f (Map m) = Map <$> traverse f m


insert :: (Ord k,Monoid v) => k -> (v -> v) -> Map k v -> Map k v
insert k f (Map m) =
  let v = M.findWithDefault mempty k m
   in Map $ M.insert k (f v) m

mapKeys :: (Ord k, Monoid v) => (k -> k) -> Map k v -> Map k v
mapKeys f (Map m) = Map $ M.mapKeysWith mappend f m

filterByKey :: (k -> Bool) -> Map k v -> Map k v
filterByKey f (Map m) = Map $ M.filterWithKey (const . f) m

filter :: (a -> Bool) -> Map k a -> Map k a
filter f (Map m)= Map $ M.filter f m

split :: (k -> Bool) -> Map k v -> (Map k v, Map k v)
split f (Map m) =
  let (m1, m2) = M.partitionWithKey (const . f) m
   in (Map m1, Map m2)

toList :: Map k v -> [(k, v)]
toList (Map m) = M.toList m

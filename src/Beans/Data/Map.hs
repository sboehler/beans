module Beans.Data.Map
  ( Map
  , accumulate
  , insert
  , insert'
  , empty
  , isEmpty
  , find
  , findWithDefault
  , findWithDefault'
  , fromList
  , mapKeys
  , filter
  , lookupLE
  , lookupLT
  , filterByKey
  , singleton
  , member
  , toList
  , split
  ) where

import           Data.Foldable         (Foldable)
import           Data.Group            (Group (..))
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict       as M
import           Data.Traversable      (Traversable (..))
import           Prelude               hiding (filter)

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

instance (Show k, Show v) => Show (Map k v) where
  show (Map m) = show m

insert :: (Ord k, Monoid v) => k -> (v -> v) -> Map k v -> Map k v
insert k f (Map m) =
  let v = M.findWithDefault mempty k m
   in Map $ M.insert k (f v) m

insert' :: Ord k => k -> v -> Map k v -> Map k v
insert' k v (Map m) = Map $ M.insert k v m

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

fromList :: (Monoid v, Ord k) => [(k, v)] -> Map k v
fromList l = Map $ M.fromListWith mappend l

member :: Ord k => k -> Map k v -> Bool
member k (Map m) = k `M.member` m

find :: Ord k => k -> Map k a -> Maybe a
find k (Map m) = M.lookup k m

findWithDefault :: (Ord k, Monoid a) => k -> Map k a -> a
findWithDefault k (Map m) = M.findWithDefault mempty k m

findWithDefault' :: (Ord k) => a -> k -> Map k a -> a
findWithDefault' d k (Map m) = M.findWithDefault d k m

isEmpty :: Map k v -> Bool
isEmpty (Map m) = null m

lookupLT :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLT k (Map m) = maybe mempty snd (M.lookupLT k m)

lookupLE :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLE k (Map m) = maybe mempty snd (M.lookupLE k m)

empty :: Map k v
empty = Map M.empty

singleton :: k -> v -> Map k v
singleton k v = Map $ M.singleton k v

accumulate :: (Monoid v, Ord k) => Map k v -> Map k v -> Map k [v]
accumulate (Map m1) (Map m2) =
  Map $
  MM.merge
    (MM.mapMissing (\_ a -> [a, mempty]))
    (MM.mapMissing (\_ a -> [mempty, a]))
    (MM.zipWithMatched (\_ x y -> [x, y]))
    m1
    m2

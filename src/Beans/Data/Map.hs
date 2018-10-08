{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beans.Data.Map
  ( Map
  , insertM
  , combineM
  , insert
  , delete
  , empty
  , lookup
  , minus
  , findWithDefaultM
  , foldlWithKey
  , fromListM
  , mapKeysM
  , mapEntries
  , mapWithKey
  , filter
  , lookupLEM
  , filterWithKey
  , singleton
  , size
  , member
  , toList
  , partitionWithKey
  ) where

import           Data.Foldable    (Foldable)
import           Data.Group       (Group (..))
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import           Data.Traversable (Traversable (..))
import           Prelude          hiding (filter, lookup)

newtype Map k v = Map
  { unmap :: M.Map k v
  } deriving (Eq, Functor, Traversable, Foldable)

instance (Semigroup v, Ord k) => Semigroup (Map k v) where
  (Map m1) <> (Map m2) = Map (M.unionWith (<>) m1 m2)

instance (Monoid v, Ord k) => Monoid (Map k v) where
  mempty = Map mempty

instance (Group v, Ord k) => Group (Map k v) where
  invert (Map v) = Map (invert <$> v)

instance (Show k, Show v) => Show (Map k v) where
  show (Map m) = show m

minus :: (Group v, Ord k) => Map k v -> Map k v -> Map k v
minus m1 m2 = m1 `mappend` (invert <$> m2)

insertM :: (Monoid v, Ord k) => k -> v -> Map k v -> Map k v
insertM k v (Map m) = Map $ M.insertWith mappend k v m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m) = Map $ M.insert k v m

mapKeysM :: (Ord k, Ord j, Monoid v) => (k -> j) -> Map k v -> Map j v
mapKeysM f (Map m) = Map $ M.mapKeysWith mappend f m

mapWithKey :: (k -> a -> v) -> Map k a -> Map k v
mapWithKey f (Map m) = Map $ M.mapWithKey f m

mapEntries
  :: (Ord k, Ord k', Monoid v, Monoid v')
  => ((k, v) -> (k', v'))
  -> Map k v
  -> Map k' v'
mapEntries f = fromListM . fmap f . toList

filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
filterWithKey f (Map m) = Map $ M.filterWithKey f m

filter :: (a -> Bool) -> Map k a -> Map k a
filter f (Map m) = Map $ M.filter f m

partitionWithKey :: (k -> v -> Bool) -> Map k v -> (Map k v, Map k v)
partitionWithKey f (Map m) =
  let (m1, m2) = M.partitionWithKey f m in (Map m1, Map m2)

toList :: Map k v -> [(k, v)]
toList = M.toList . unmap

fromListM :: (Monoid v, Ord k) => [(k, v)] -> Map k v
fromListM l = Map $ M.fromListWith mappend l

member :: Ord k => k -> Map k v -> Bool
member k (Map m) = k `M.member` m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (Map m) = M.lookup k m

findWithDefaultM :: (Ord k, Monoid a) => k -> Map k a -> a
findWithDefaultM k (Map m) = M.findWithDefault mempty k m

lookupLEM :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLEM k (Map m) = maybe mempty snd (M.lookupLE k m)

empty :: Map k v
empty = Map M.empty

size :: Map k a -> Int
size = M.size . unmap

singleton :: k -> v -> Map k v
singleton k v = Map $ M.singleton k v

combineM :: (Monoid v, Ord k) => [Map k v] -> Map k [v]
combineM maps =
  let k = S.toList $ mconcat (M.keysSet . unmap <$> maps)
      m = [ (account, findWithDefaultM account <$> maps) | account <- k ]
  in  Map $ M.fromList m

foldlWithKey :: (c -> k -> b -> c) -> c -> Map k b -> c
foldlWithKey f z = M.foldlWithKey f z . unmap

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m) = Map $ M.delete k m

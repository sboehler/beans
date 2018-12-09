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
  , keys
  , elems
  , minus
  , filterKeys
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
  , partitionKeys
  )
where

import           Data.Foldable                  ( Foldable )
import           Data.Group                     ( Group(..) )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Traversable               ( Traversable(..) )
import           Prelude                 hiding ( filter
                                                , lookup
                                                )

newtype Map k v = Map
  { unmap :: M.Map k v
  } deriving (Eq, Ord, Functor, Traversable, Foldable)

instance (Semigroup v, Ord k) => Semigroup (Map k v) where
  (Map m1) <> (Map m2) = Map (M.unionWith (<>) m1 m2)

instance (Monoid v, Ord k) => Monoid (Map k v) where
  mempty = Map mempty

instance (Group v, Ord k) => Group (Map k v) where
  invert = fmap invert

instance (Show k, Show v) => Show (Map k v) where
  show = show . unmap

minus :: (Group v, Ord k) => Map k v -> Map k v -> Map k v
minus m1 m2 = m1 `mappend` (invert <$> m2)

insertM :: (Monoid v, Ord k) => k -> v -> Map k v -> Map k v
insertM k v (Map m) = Map $ M.insertWith mappend k v m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = Map . M.insert k v . unmap

mapKeysM :: (Ord k, Ord j, Monoid v) => (k -> j) -> Map k v -> Map j v
mapKeysM f = Map . M.mapKeysWith mappend f . unmap

mapWithKey :: (k -> a -> v) -> Map k a -> Map k v
mapWithKey f = Map . M.mapWithKey f . unmap

mapEntries
  :: (Ord k, Ord k', Monoid v, Monoid v')
  => ((k, v) -> (k', v'))
  -> Map k v
  -> Map k' v'
mapEntries f = fromListM . fmap f . toList

filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
filterWithKey f = Map . M.filterWithKey f . unmap

filter :: (a -> Bool) -> Map k a -> Map k a
filter f = Map . M.filter f . unmap

filterKeys :: (k -> Bool) -> Map k v -> Map k v
filterKeys f = filterWithKey (\k _ -> f k)

partitionWithKey :: (k -> v -> Bool) -> Map k v -> (Map k v, Map k v)
partitionWithKey f (Map m) =
  let (m1, m2) = M.partitionWithKey f m in (Map m1, Map m2)

partitionKeys :: (k -> Bool) -> Map k v -> (Map k v, Map k v)
partitionKeys f = partitionWithKey (\k _ -> f k)

toList :: Map k v -> [(k, v)]
toList = M.toList . unmap

fromListM :: (Monoid v, Ord k) => [(k, v)] -> Map k v
fromListM l = Map $ M.fromListWith mappend l

member :: Ord k => k -> Map k v -> Bool
member k = (k `M.member`) . unmap

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k = M.lookup k . unmap

findWithDefaultM :: (Ord k, Monoid a) => k -> Map k a -> a
findWithDefaultM k = M.findWithDefault mempty k . unmap

lookupLEM :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLEM k = maybe mempty snd . M.lookupLE k . unmap

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
delete k = Map . M.delete k . unmap

keys :: Map k a -> [k]
keys = M.keys . unmap

elems :: Map k a -> [a]
elems = M.elems . unmap

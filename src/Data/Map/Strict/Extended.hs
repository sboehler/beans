module Data.Map.Strict.Extended
  ( module Data.Map.Strict
  , lookupLessThan
  , lookupLessEqual
  , toListWith
  , mapMonoid
  ) where

import           Data.Map.Strict

lookupLessThan :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLessThan k m = maybe mempty snd (lookupLT k m)

lookupLessEqual :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLessEqual k m = maybe mempty snd (lookupLE k m)

toListWith :: (Ord k) => ((k, v) -> a) -> Map k v -> [a]
toListWith f = fmap f . toList

mapMonoid :: (Ord k, Monoid m) => k -> (m -> m) -> Map k m -> Map k m
mapMonoid k f m =
  let m1 = findWithDefault mempty k m
   in insert k (f m1) m

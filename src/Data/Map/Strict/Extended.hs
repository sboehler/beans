module Data.Map.Strict.Extended
  ( module Data.Map.Strict
  , lookupLessThan
  , lookupLessEqual
  ) where

import Data.Map.Strict

lookupLessThan :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLessThan k m = maybe mempty snd (lookupLT k m)

lookupLessEqual :: (Monoid v, Ord k) => k -> Map k v -> v
lookupLessEqual k m = maybe mempty snd (lookupLE k m)

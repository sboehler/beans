module Data.Map.Strict.Extended
  ( module Data.Map.Strict,
    -- traverseWithKey,
    foldMWithKey,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict

foldMWithKey :: (Ord k, Monad m) => (a -> k -> v -> m a) -> a -> Map k v -> m a
foldMWithKey f a0 = foldM g a0 . toList
  where
    g a (k, v) = f a k v

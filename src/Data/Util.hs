module Data.Util
  ( adjustWithDefault
  ) where

import qualified Data.Map.Lazy as M

adjustWithDefault ::
     (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault f = M.alter f'
  where
    f' (Just a) = Just $ f a
    f' Nothing = Just $ f mempty

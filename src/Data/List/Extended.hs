module Data.List.Extended
  ( module Data.List,
    groupByKey,
    groupByKey',
  )
where

import Data.List

groupByKey :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupByKey _ [] = []
groupByKey f (x : xs) = (f x, x : ys) : groupByKey f zs
  where
    (ys, zs) = span ((== f x) . f) xs

groupByKey' :: (k -> k -> Bool) -> [(k, a)] -> [(k, [a])]
groupByKey' _ [] = []
groupByKey' eq ((k, v) : xs) = (k, v : fmap snd ys) : groupByKey' eq zs
  where
    (ys, zs) = span (eq k . fst) xs

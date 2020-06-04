module Beans.ATree (ATree (ATree), fromList) where

import qualified Data.Foldable as Foldable
import qualified Data.List.Extended as List
import qualified Data.Maybe as Maybe
import Prelude

data ATree k v = ATree k v [ATree k v]
  deriving (Show)

instance Foldable (ATree k) where
  foldMap f (ATree _ v s) = f v <> foldMap (foldMap f) s

instance Functor (ATree k) where
  fmap f (ATree k v st) = ATree k (f v) [fmap f t | t <- st]

fromList :: (Eq k, Monoid k, Monoid v) => [([k], v)] -> [ATree k v]
fromList l = children t
  where
    t = fromList' (mempty, l)
    children (ATree _ _ ch) = ch

fromList' :: (Eq k, Monoid v) => (k, [([k], v)]) -> ATree k v
fromList' (k, l) = ATree k value (fromList' <$> sublists)
  where
    (justs, nothings) = List.partition (Maybe.isJust . fst) . fmap shift $ l
    value = Foldable.fold $ snd . unshift <$> nothings
    sublists = fmap (\(Just k', vs) -> (k', unshift <$> vs)) . List.groupByKey fst $ justs

shift :: ([k], v) -> (Maybe k, ([k], v))
shift (k : ks, value) = (Just k, (ks, value))
shift ([], value) = (Nothing, ([], value))

unshift :: (Maybe k, ([k], v)) -> ([k], v)
unshift = snd

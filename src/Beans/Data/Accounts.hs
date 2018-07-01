module Beans.Data.Accounts where

import           Beans.AST       (AccountName, CommodityName, Lot)
import qualified Beans.Data.Map  as M
import           Data.Foldable   (fold)
import           Data.Group      (invert)
import           Data.Monoid     (Sum (Sum), getSum)
import           Data.Scientific (Scientific)

type Accounts = M.Map (AccountName, CommodityName, Maybe Lot) (Sum Scientific)

add :: AccountName -> CommodityName -> Maybe Lot -> Scientific -> Accounts -> Accounts
add a c l s = M.insert (a, c, l) (mappend (Sum s))

minus :: Accounts -> Accounts -> Accounts
minus a1 a2 = a1 `mappend` invert a2

mapAccounts :: (AccountName -> AccountName) -> Accounts -> Accounts
mapAccounts f = M.mapKeys (\(a, c, l) -> (f a, c, l))

mapLots :: (Maybe Lot -> Maybe Lot) -> Accounts -> Accounts
mapLots f = M.mapKeys (\(a, c, l) -> (a, c, f l))

balance :: AccountName -> CommodityName -> Accounts -> Scientific
balance a c = getSum . fold . filter'
  where
    filter' = M.filterByKey (\(a', c', _) -> a' == a && c' == c)

filter :: (Scientific -> Bool) -> Accounts -> Accounts
filter f = M.filter (f . getSum)

split :: AccountName -> Accounts -> (Accounts, Accounts)
split a = M.split (\(a', _, _) -> (a == a'))

toList ::
     Accounts -> [((AccountName, CommodityName, Maybe Lot), Scientific)]
toList = M.toList . fmap getSum

module Beans.Balance
  ( Balance (Balance),
    book,
    eraseLots,
    inPercent,
    new,
    diff,
  )
where

import Beans.Account (Account)
import qualified Beans.Account as Account
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import Beans.Filter (AccountFilter)
import Beans.Position (Position (..))
import qualified Beans.Position as Position
import Beans.Positions (Positions)
import qualified Beans.Positions as Positions
import Beans.Prices (NormalizedPrices, Prices)
import qualified Beans.Prices as Prices
import Beans.Transaction (Posting (..))
import Beans.ValAmount (ValAmount (ValAmount))
import Data.Foldable (fold)
import Data.Group (Group, invert)
import qualified Data.Map.Strict.Extended as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Balance a
  = Balance
      Date
      (Positions a)
      (Set Account)
      Prices
      [NormalizedPrices]
  deriving (Show, Functor)

new :: Date -> [Commodity] -> Balance a
new date commodities =
  Balance
    date
    Map.empty
    (Set.singleton Account.valuationAccount)
    Map.empty
    (Prices.newN <$> commodities)

diff :: (Monoid a, Group a) => Balance a -> Balance a -> Balance a
diff (Balance d p1 a pr np) (Balance _ p0 _ _ _) = Balance d p' a pr np
  where
    p' = Map.unionWith (<>) p1 (invert <$> p0)

book :: Balance ValAmount -> [Posting] -> Balance ValAmount
book (Balance d p ac pr np) ps = Balance d (Map.unionWith (<>) p pos) ac pr np
  where
    pos = Positions.fromList [(Position a c l, amt) | Posting a c l amt _ <- ps]

eraseLots :: Monoid a => Balance a -> Balance a
eraseLots (Balance d p ac pr np) = Balance d p' ac pr np
  where
    p' = Map.mapKeysWith (<>) Position.deleteLot p

inPercent :: AccountFilter -> Balance ValAmount -> Balance ValAmount
inPercent f (Balance d p a pr np) = Balance d (safediv <$> p) a pr np
  where
    ValAmount _ total = fold $ Map.filterWithKey match p
    match (Position account _ _) = const $ Account.match f account
    safediv (ValAmount am v) =
      ValAmount
        am
        ( if null total
            then fmap (const 0) v
            else Map.unionWith (\x y -> if y == 0 then 0 else x / y) v total
        )

module Beans.Positions
  ( Positions,
    accounts,
    concatenate,
    filterByAccountType,
    fromList,
    new,
    partitionByAccount,
    sum,
    valuate,
  )
where

import Beans.Account (Account (Account), AccountType)
import Beans.Amount (Amount)
import Beans.Commodity (Commodity)
import Beans.Position (Position (..))
import Beans.ValAmount (ValAmount)
import qualified Beans.ValAmount as ValAmount
import Control.Monad.Catch (MonadThrow)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict.Extended (Map)
import qualified Data.Map.Strict.Extended as Map
import Prelude hiding (filter, sum)

type Positions a = Map Position a

new :: Positions a
new = Map.empty

accounts :: Positions a -> [Account]
accounts pos = (\(Position a _ _) -> a) <$> Map.keys pos

fromList :: Monoid a => [(Position, a)] -> Positions a
fromList = Map.fromListWith (<>)

sum :: Monoid a => Account -> Commodity -> Positions a -> a
sum a c = fold . Map.filterWithKey g
  where
    g (Position a' c' _) _ = a' == a && c' == c

filterByAccountType :: [AccountType] -> Positions a -> Positions a
filterByAccountType t = Map.filterWithKey g
  where
    g (Position (Account t' _) _ _) _ = t' `elem` t

partitionByAccount :: Account -> Positions a -> (Positions a, Positions a)
partitionByAccount a = Map.partitionWithKey (\(Position a' _ _) _ -> a' == a)

valuate :: MonadThrow m => Commodity -> (Commodity -> Amount -> m Amount) -> Positions ValAmount -> m (Positions ValAmount)
valuate tc f = Map.traverseWithKey valuate' . coerce
  where
    valuate' (Position _ c _) = ValAmount.valuate tc (f c)

concatenate :: Monoid a => [Positions a] -> Map Position [a]
concatenate positions = pos
  where
    (_, pos) = foldr g (0, mempty) positions
    g v (n, vs) = (n + 1, merge n vs v)

merge :: Monoid a => Int -> Map Position [a] -> Map Position a -> Map Position [a]
merge n = Map.merge (Map.mapMissing g1) (Map.mapMissing g2) (Map.zipWithMatched m)
  where
    g1 _ existing = mempty : existing
    g2 _ amt = amt : replicate n mempty
    m _ existing amt = amt : existing

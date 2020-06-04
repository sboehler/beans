module Beans.Amounts where

import Beans.Commodity (Commodity)
import Beans.ValAmount (ValAmount)
import Data.Coerce (coerce)
import Data.Group (Group (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc (Pretty, pretty)
import Prelude hiding (filter)

newtype Amounts = Amounts (Map Commodity ValAmount)
  deriving (Eq, Show)

instance Semigroup Amounts where
  Amounts a1 <> Amounts a2 = Amounts $ Map.unionWith (<>) a1 a2

instance Monoid Amounts where
  mempty = Amounts Map.empty

instance Group Amounts where
  invert = Amounts . fmap invert . coerce

instance Pretty Amounts where
  pretty = pretty . show

fromList :: [(Commodity, ValAmount)] -> Amounts
fromList = Amounts . Map.fromListWith (<>)

size :: Amounts -> Int
size (Amounts m) = Map.size m

fromAmount :: Commodity -> ValAmount -> Amounts
fromAmount c a = Amounts $ Map.singleton c a

filter :: (Commodity -> ValAmount -> Bool) -> Amounts -> Amounts
filter f = Amounts . Map.filterWithKey f . coerce

toList :: Amounts -> [(Commodity, ValAmount)]
toList = Map.toList . coerce

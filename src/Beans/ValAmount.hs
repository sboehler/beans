module Beans.ValAmount
  ( ValAmount (ValAmount),
    showFixed,
    amountIsZero,
    valuate,
    new,
  )
where

import Beans.Amount (Amount)
import qualified Beans.Amount as Amount
import Beans.Commodity (Commodity)
import Beans.Prices (NormalizedPrices (NormalizedPrices))
import qualified Beans.Prices as Prices
import Control.Monad.Catch (MonadThrow)
import Data.Group (Group (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Pretty (pretty))

data ValAmount
  = ValAmount Amount (Map Commodity Amount)
  deriving (Eq)

instance Show ValAmount where
  show (ValAmount a _) = Text.unpack . Amount.showFixed $ a

instance Semigroup ValAmount where
  ValAmount a1 v1 <> ValAmount a2 v2 = ValAmount (a1 + a2) (Map.unionWith (<>) v1 v2)

instance Monoid ValAmount where
  mempty = new 0

instance Group ValAmount where
  invert (ValAmount a v) = ValAmount (- a) (negate <$> v)

instance Pretty ValAmount where
  pretty (ValAmount a _) = pretty a

new :: Amount -> ValAmount
new a = ValAmount a Map.empty

showFixed :: ValAmount -> Text
showFixed (ValAmount a _) = Amount.showFixed a

valuate :: MonadThrow m => Commodity -> NormalizedPrices -> ValAmount -> m ValAmount
valuate c np@(NormalizedPrices tc _) (ValAmount a v) = do
  val <- Prices.valuate np c a
  pure $ ValAmount a (Map.insert tc val v)

amountIsZero :: ValAmount -> Bool
amountIsZero (ValAmount 0 _) = True
amountIsZero _ = False

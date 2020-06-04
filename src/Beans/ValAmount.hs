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

valuate :: Monad m => Commodity -> (Amount -> m Amount) -> ValAmount -> m ValAmount
valuate c f (ValAmount a v) = do
  v' <- Map.insert c <$> f a <*> pure v
  pure $ ValAmount a v'

amountIsZero :: ValAmount -> Bool
amountIsZero (ValAmount 0 _) = True
amountIsZero _ = False

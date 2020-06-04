module Beans.Prices
  ( NormalizedPrices,
    Prices,
    updatePrices,
    normalize,
    valuate,
    new,
    newN,
    lookupPrice,
  )
where

import Beans.Amount (Amount)
import qualified Beans.Amount as Amount
import Beans.Commodity (Commodity)
import Beans.Price (Price (..))
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type NormalizedPrices = Map Commodity Double

type Prices = Map Commodity (Map Commodity Double)

data PriceException
  = NoPriceFound Commodity Commodity
  | NoNormalizedPriceFound NormalizedPrices Commodity
  deriving (Show)

instance Exception PriceException

new :: Prices
new = Map.empty

newN :: NormalizedPrices
newN = Map.empty

updatePrices :: MonadThrow m => Prices -> Price -> m Prices
updatePrices prices (Price _ c p tc) =
  pure prices
    >>= addPrice c tc p
    >>= addPrice tc c (1 / p)

addPrice :: MonadThrow m => Commodity -> Commodity -> Double -> Prices -> m Prices
addPrice commodity targetCommodity price prices = do
  let inner = Map.findWithDefault mempty commodity prices
  let inner' = Map.insert targetCommodity price inner
  pure $ Map.insert commodity inner' prices

normalize :: Prices -> Commodity -> NormalizedPrices
normalize prices current = normalize' prices mempty (Map.singleton current 1.0)

normalize' :: Prices -> NormalizedPrices -> NormalizedPrices -> NormalizedPrices
normalize' prices done todo =
  case Map.lookupMin todo of
    Just (c, p) ->
      let done' = Map.insert c p done
       in case Map.lookup c prices of
            Nothing -> done'
            Just neighbors ->
              let neighbors' = (* p) <$> (neighbors `Map.difference` done')
                  todo' = Map.delete c todo `Map.union` neighbors'
               in normalize' prices done' todo'
    Nothing -> done

lookupPrice :: (MonadThrow m) => Commodity -> NormalizedPrices -> m Double
lookupPrice commodity prices = case Map.lookup commodity prices of
  Just p -> pure $ 1 / p
  _ -> throwM $ NoNormalizedPriceFound prices commodity

valuate :: MonadThrow m => NormalizedPrices -> Commodity -> Amount -> m Amount
valuate np c a = do
  p <- lookupPrice c np
  pure $ Amount.asFloat (* p) a

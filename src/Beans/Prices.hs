module Beans.Prices
  ( PricesHistory
  , NormalizedPrices
  , Prices
  , updatePrices
  , normalize
  , lookupPrice
  )
where

import           Beans.Model                    ( Command(..)
                                                , Price(..)
                                                , Commodity(..)
                                                )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import qualified Data.Map.Strict               as M
import           Data.Scientific                ( Scientific
                                                , fromFloatDigits
                                                , toRealFloat
                                                )
import           Data.Time.Calendar             ( Day )

type PricesHistory = M.Map Day Prices

type NormalizedPrices = M.Map Commodity Scientific

type Prices = M.Map Commodity (M.Map Commodity Scientific)

data PriceException
  = NoPriceFound Commodity
                 Commodity
  | NoNormalizedPriceFound NormalizedPrices
                           Commodity
  deriving (Show)

instance Exception PriceException

updatePrices :: Prices -> Command -> Prices
updatePrices prices (CmdPrice p) =
  let prices' = addPrice prices p in addPrice prices' (invert p)
updatePrices p _ = p

addPrice :: Prices -> Price -> Prices
addPrice prices Price { prCommodity, prPrice, prTargetCommodity } =
  let p = M.findWithDefault mempty prCommodity prices
  in  M.insert prCommodity (M.insert prTargetCommodity prPrice p) prices

invert :: Price -> Price
invert Price { prCommodity, prTargetCommodity, prPrice } = Price
  { prCommodity       = prTargetCommodity
  , prTargetCommodity = prCommodity
  , prPrice           = 1 `sdiv` prPrice
  }

normalize :: Prices -> Commodity -> NormalizedPrices
normalize prices current = normalize' prices mempty (M.singleton current 1.0)

normalize' :: Prices -> NormalizedPrices -> NormalizedPrices -> NormalizedPrices
normalize' prices done todo = case M.lookupMin todo of
  Nothing -> done
  Just (commodity, price) ->
    let done' = M.insert commodity price done
    in  case M.lookup commodity prices of
          Nothing -> done'
          Just neighbors ->
            let neighbors' = (* price) <$> (neighbors `M.difference` done')
                todo'      = M.delete commodity todo `M.union` neighbors'
            in  normalize' prices done' todo'

lookupPrice :: (MonadThrow m) => Commodity -> NormalizedPrices -> m Scientific
lookupPrice commodityName prices = case M.lookup commodityName prices of
  Just p -> return $ 1 `sdiv` p
  _      -> throwM $ NoNormalizedPriceFound prices commodityName

sdiv :: Scientific -> Scientific -> Scientific
sdiv x y = fromFloatDigits (toRealFloat x / toRealFloat y :: Double)

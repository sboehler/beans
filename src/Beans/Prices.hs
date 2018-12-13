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
import           Control.Monad.State            ( StateT
                                                , get
                                                , put
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

updatePrices :: Monad m => Command -> StateT Prices m ()
updatePrices (CmdPrice p) = do
  prices <- get
  let prices'  = addPrice prices p
      prices'' = addPrice prices' (invert p)
  put prices''
updatePrices _ = pure ()

addPrice :: Prices -> Price -> Prices
addPrice prices Price { _priceCommodity, _pricePrice, _priceTargetCommodity } =
  let p = M.findWithDefault mempty _priceCommodity prices
  in  M.insert _priceCommodity
               (M.insert _priceTargetCommodity _pricePrice p)
               prices

invert :: Price -> Price
invert Price { _priceCommodity, _priceTargetCommodity, _pricePrice } = Price
  { _priceCommodity       = _priceTargetCommodity
  , _priceTargetCommodity = _priceCommodity
  , _pricePrice           = 1 `sdiv` _pricePrice
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

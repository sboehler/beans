module Beans.Prices
  ( PricesHistory
  , NormalizedPrices
  , Prices
  , updatePrices
  , normalize
  , lookupPrice
  )
where

import           Beans.Model
import           Control.Monad.State            ( StateT
                                                , modify
                                                , gets
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
import           Control.Lens

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
updatePrices (CmdPrice p) = addPrice p >> addPrice (invert p)
updatePrices _            = pure ()

addPrice :: Monad m => Price -> StateT Prices m ()
addPrice p = do
  inner <- gets $ M.findWithDefault mempty (p ^. commodity)
  let inner' = M.insert (p ^. targetCommodity) (p ^. price) inner
  modify $ M.insert (p ^. commodity) inner'

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
  Just (c, p) ->
    let done' = M.insert c p done
    in  case M.lookup c prices of
          Nothing -> done'
          Just neighbors ->
            let neighbors' = (* p) <$> (neighbors `M.difference` done')
                todo'      = M.delete c todo `M.union` neighbors'
            in  normalize' prices done' todo'

lookupPrice :: (MonadThrow m) => Commodity -> NormalizedPrices -> m Scientific
lookupPrice commodityName prices = case M.lookup commodityName prices of
  Just p -> return $ 1 `sdiv` p
  _      -> throwM $ NoNormalizedPriceFound prices commodityName

sdiv :: Scientific -> Scientific -> Scientific
sdiv x y = fromFloatDigits (toRealFloat x / toRealFloat y :: Double)

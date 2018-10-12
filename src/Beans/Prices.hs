module Beans.Prices
  ( PricesHistory
  , NormalizedPrices
  , Prices
  , updatePrices
  , normalize
  , lookupPrice
  )
where

import           Beans.Data.Accounts                      ( Commodity(..) )
import           Beans.Data.Directives                    ( Command(..) )
import           Control.Monad.Catch                      ( Exception
                                                          , MonadThrow
                                                          , throwM
                                                          )
import qualified Data.Map.Strict               as M
import           Data.Scientific                          ( Scientific
                                                          , fromFloatDigits
                                                          , toRealFloat
                                                          )
import           Data.Time.Calendar                       ( Day )

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
updatePrices prices command =
  let prices' = addPrice prices command in addPrice prices' (invert command)

addPrice :: Prices -> Command -> Prices
addPrice prices Price { pCommodity, pPrice, pTargetCommodity } =
  let p = M.findWithDefault mempty pCommodity prices
  in  M.insert pCommodity (M.insert pTargetCommodity pPrice p) prices
addPrice prices _ = prices

invert :: Command -> Command
invert Price { pCommodity, pTargetCommodity, pPrice } = Price
  { pCommodity       = pTargetCommodity
  , pTargetCommodity = pCommodity
  , pPrice           = 1 `sdiv` pPrice
  }
invert c = c

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

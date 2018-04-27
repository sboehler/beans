module Haricot.Prices
  ( PricesHistory
  , Prices
  , calculatePrices
  , getPrice
  ) where

import           Control.Exception
import           Control.Monad.Catch(MonadThrow, throwM)
import qualified Data.Map.Strict    as M
import           Data.Scientific    (Scientific)
import           Data.Time.Calendar (Day)
import           Haricot.AST
import           Haricot.Ledger

type PricesHistory = M.Map Day Prices

type Prices = M.Map CommodityName (M.Map CommodityName Scientific)

data PriceException =
  NoPriceFound CommodityName
               CommodityName
  deriving (Show)
instance Exception PriceException

calculatePrices :: Ledger -> PricesHistory
calculatePrices l = fst $ foldl f (M.empty, M.empty) l
  where
    f (pricesHistory, prices) ts@Timestep {_date} =
      let prices' = updatePrices ts prices
       in (M.insert _date prices' pricesHistory, prices')

updatePrices :: Timestep -> Prices -> Prices
updatePrices Timestep {..} prices = foldr addPrice prices _prices

addPrice :: Price -> Prices -> Prices
addPrice Price {..} prices =
  let p = M.findWithDefault M.empty _commodity prices
      q = M.findWithDefault M.empty _targetCommodity prices
      prices' = M.insert _commodity (M.insert _targetCommodity _price p) prices
   in M.insert _targetCommodity (M.insert _commodity (1 / _price) q) prices'

getPrice :: MonadThrow m => Prices -> CommodityName -> CommodityName -> m Scientific
getPrice prices source target =
  let v = find prices source target (M.fromList [(source, 1.0)]) []
   in case M.lookup target v of
        Just p -> return p
        _ -> throwM $ NoPriceFound source target

find ::
     Prices
  -> CommodityName
  -> CommodityName
  -> M.Map CommodityName Scientific
  -> [CommodityName]
  -> M.Map CommodityName Scientific
find prices current target visited queue =
  if current == target
    then visited
    else case M.lookup current prices of
           Just m ->
             let p = M.findWithDefault 1 current visited
                 neighbors = fmap (* p) . M.filter (`notElem` visited) $ m
                 visited' = M.union visited neighbors
                 queue' = queue ++ M.keys neighbors
              in case queue' of
                   (current':qs) -> find prices current' target visited' qs
                   [] -> visited
           Nothing -> visited

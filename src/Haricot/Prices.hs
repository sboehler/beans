module Haricot.Prices
  ( PricesHistory
  , Prices
  , calculatePrices
  ) where

import qualified Data.Map.Strict    as M
import           Data.Scientific    (Scientific)
import           Data.Time.Calendar (Day)
import           Haricot.AST
import           Haricot.Ledger

type PricesHistory = M.Map Day Prices

type Prices = M.Map CommodityName (M.Map CommodityName Scientific)

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
   in M.insert _commodity (M.insert _targetCommodity _price p) prices

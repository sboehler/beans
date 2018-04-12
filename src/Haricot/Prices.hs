module Haricot.Prices
  (
   PricesHistory
  , Prices
  , calculatePrices
  ) where

import qualified Data.Map.Strict    as M
import           Data.Scientific
import           Data.Time.Calendar (Day)
import           Haricot.AST
import           Haricot.Ledger

type PricesHistory = M.Map Day Prices

type Prices = M.Map CommodityName (M.Map CommodityName Scientific)

calculatePrices :: Ledger -> PricesHistory
calculatePrices l = fst $ foldr f (M.empty, M.empty) l
  where
    f ts@Timestep {_date} (accountsHistory, latest) =
      let latest' = updatePrices ts latest
       in (M.insert _date latest' accountsHistory, latest')

updatePrices ::  Timestep -> Prices -> Prices
updatePrices Timestep {..} prices = foldr addPrice prices _prices

addPrice :: Price -> Prices -> Prices
addPrice Price {..} prices =
  let p = M.findWithDefault M.empty _commodity prices
   in M.insert _commodity (M.insert _targetCommodity _price p) prices

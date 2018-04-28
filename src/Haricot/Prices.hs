module Haricot.Prices
  ( PricesHistory
  , Prices
  , calculatePrices
  , updatePrices
  , getPrice
  ) where

import           Control.Exception
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.State (evalState, get, modify)
import           Data.Foldable       (foldl')
import qualified Data.Map.Strict     as M
import           Data.Scientific     (Scientific, fromFloatDigits, toRealFloat)
import           Data.Time.Calendar  (Day)
import           Haricot.AST
import           Haricot.Ledger

type PricesHistory = M.Map Day Prices

type Prices = M.Map CommodityName (M.Map CommodityName Scientific)

data PriceException =
  NoPriceFound CommodityName
               CommodityName
  deriving (Show)

instance Exception PriceException

calculatePrices :: Traversable t => t Timestep -> t Prices
calculatePrices l = evalState (mapM f l) mempty
  where
    f timestep = modify (updatePrices timestep) >> get

updatePrices :: Timestep -> Prices -> Prices
updatePrices Timestep {_prices} prices =
  let prices' = foldl' addPrice prices _prices
   in foldl' (\p -> addPrice p . invert) prices' _prices

addPrice :: Prices -> Price -> Prices
addPrice prices Price {..} =
  let p = M.findWithDefault mempty _commodity prices
   in M.insert _commodity (M.insert _targetCommodity _price p) prices

invert :: Price -> Price
invert p@Price {..} =
  p
    { _commodity = _targetCommodity
    , _targetCommodity = _commodity
    , _price = fromFloatDigits (1 / toRealFloat _price :: Double)
    }

getPrice ::
     MonadThrow m => Prices -> CommodityName -> CommodityName -> m Scientific
getPrice prices source target =
  let v = find prices source target (M.fromList [(source, 1.0)]) []
   in case M.lookup target v of
        Just p -> return p
        _      -> throwM $ NoPriceFound source target

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
                 visited' = visited `M.union` neighbors
                 queue' = queue ++ M.keys neighbors
              in case queue' of
                   (current':qs) -> find prices current' target visited' qs
                   []            -> visited
           Nothing -> visited

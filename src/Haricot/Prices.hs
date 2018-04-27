module Haricot.Prices
  ( PricesHistory
  , Prices
  , calculatePrices
  , getPrice
  ) where

import           Control.Exception
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.State.Strict (MonadState, evalState, get, gets,
                                             modify)
import qualified Data.Map.Strict            as M
import           Data.Scientific            (Scientific, toRealFloat, fromFloatDigits)
import           Data.Time.Calendar         (Day)
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
calculatePrices l = evalState (mapM updatePrices l) mempty

updatePrices :: (MonadState Prices m) => Timestep -> m Prices
updatePrices Timestep {_prices} =
  mapM_ addPrice _prices  >>
  mapM_ (addPrice . invert) _prices >>
  get 

addPrice :: (MonadState Prices m) => Price -> m ()
addPrice Price {..} = do
  p <- gets (M.findWithDefault mempty _commodity)
  modify $ M.insert _commodity (M.insert _targetCommodity _price p)

invert :: Price -> Price
invert p@Price {..} =
  p
    { _commodity = _targetCommodity
    , _targetCommodity = _commodity
    , _price = fromFloatDigits (1 / toRealFloat _price :: Double)
    }

getPrice :: MonadThrow m => Prices -> CommodityName -> CommodityName -> m Scientific
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
                   [] -> visited
           Nothing -> visited

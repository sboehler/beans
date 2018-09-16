module Beans.Prices
  ( PricesHistory
  , NormalizedPrices
  , Prices
  , calculatePrices
  , updatePrices
  , normalize
  , lookupPrice
  ) where

import           Beans.Data.Accounts   (CommodityName (..))
import           Beans.Data.Directives (Command (..), Price (..))
import           Beans.Ledger          (Timestep (..))
import           Control.Monad.Catch   (Exception, MonadThrow, throwM)
import           Control.Monad.State   (evalState, get, modify)
import           Data.Foldable         (foldl')
import qualified Data.List             as L
import qualified Data.Map.Strict       as M
import           Data.Scientific       (Scientific, fromFloatDigits,
                                        toRealFloat)
import           Data.Time.Calendar    (Day)

type PricesHistory = M.Map Day Prices

type NormalizedPrices = M.Map CommodityName Scientific

type Prices = M.Map CommodityName (M.Map CommodityName Scientific)


data PriceException
  = NoPriceFound CommodityName
                 CommodityName
  | NoNormalizedPriceFound NormalizedPrices
                           CommodityName
  deriving (Show)

instance Exception PriceException

calculatePrices :: Traversable t => t Timestep -> t Prices
calculatePrices l = evalState (mapM f l) mempty
  where
    f timestep = modify (updatePrices timestep) >> get

updatePrices :: Timestep -> Prices -> Prices
updatePrices (Timestep _ commands) prices =
  let prices' = foldl' addPrice prices commands
   in foldl' (\p -> addPrice p . invert) prices' commands

addPrice :: Prices -> Command -> Prices
addPrice prices (PriceCommand Price {pCommodity, pPrice, pTargetCommodity}) =
  let p = M.findWithDefault mempty pCommodity prices
   in M.insert pCommodity (M.insert pTargetCommodity pPrice p) prices
addPrice prices _ = prices

invert :: Command -> Command
invert (PriceCommand p@Price {..}) =
  PriceCommand $ p
    { pCommodity = pTargetCommodity
    , pTargetCommodity = pCommodity
    , pPrice = 1 `sdiv` pPrice
    }
invert c = c

normalize :: Prices -> CommodityName -> NormalizedPrices
normalize prices current =
  normalize' prices current (M.fromList [(current, 1.0)]) []

normalize' ::
     Prices
  -> CommodityName
  -> NormalizedPrices
  -> [CommodityName]
  -> NormalizedPrices
normalize' prices current visited queue =
  case M.lookup current prices of
    Just m ->
      let p = M.findWithDefault 1 current visited
          neighbors = fmap (* p) (m `M.difference` visited)
          visited' = visited `M.union` neighbors
          queue' = queue `L.union` M.keys neighbors
       in case queue' of
            (current':qs) -> normalize' prices current' visited' qs
            []            -> visited
    Nothing -> visited

lookupPrice ::
     (MonadThrow m) => CommodityName -> NormalizedPrices -> m Scientific
lookupPrice commodityName prices =
  case M.lookup commodityName prices of
    Just p -> return $ 1 `sdiv` p
    _      -> throwM $ NoNormalizedPriceFound prices commodityName

sdiv :: Scientific -> Scientific -> Scientific
sdiv x y = fromFloatDigits (toRealFloat x / toRealFloat y :: Double)

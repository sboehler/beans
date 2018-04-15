module Haricot.Prices
  ( PricesHistory
  , Prices
  , calculatePrices
  , getPrice
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Foldable       (asum)
import qualified Data.Map.Strict     as M
import           Data.Scientific     (Scientific)
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

getPrice :: MonadThrow m => Prices -> CommodityName -> CommodityName -> m Scientific
getPrice prices c t= case getPrice' prices c t of
  Just p  -> return p
  Nothing -> throwM $ NoPriceFound c t


getPrice' :: Prices -> CommodityName -> CommodityName -> Maybe Scientific
getPrice' prices commodity targetCommodity = do
  m <- M.lookup commodity prices
  M.lookup targetCommodity m <|> asum (M.mapWithKey f m)
  where
    f c p = (p *) <$> getPrice prices' c targetCommodity
    prices' = M.delete commodity prices

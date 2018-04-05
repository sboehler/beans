module Haricot.Holdings where

import           Data.Map.Strict as M
import           Data.Scientific (Scientific)
import           Haricot.AST
import Control.Monad.Catch
import Haricot.Ledger

type Holdings = M.Map AccountName (M.Map (Maybe Lot) Scientific)

updateHoldings ::
     MonadThrow m => (AccountName -> CommodityName -> Bool) -> Holdings -> Timestep -> m Holdings
updateHoldings isOpen holdings timestep = undefined

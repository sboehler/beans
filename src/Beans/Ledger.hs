module Beans.Ledger where

import           Beans.Data.Ledger  (Balance (..), Close (..), Directive (..),
                                     Open (..), Price (..), Transaction (..))
import qualified Data.Map.Strict    as M
import           Data.Time.Calendar (Day)


data Timestep = Timestep
  { _date         :: Day,
    _openings     :: [Open]
  , _closings     :: [Close]
  , _balances     :: [Balance]
  , _transactions :: [Transaction]
  , _prices       :: [Price]
  } deriving (Show)

type Ledger = M.Map Day Timestep

buildLedger :: [Directive] -> Ledger
buildLedger = foldr updateLedger M.empty

updateLedger :: Directive  -> Ledger -> Ledger
updateLedger directive ledger =
  case date directive of
    Just day ->
      let t = M.findWithDefault (Timestep day [] [] [] [] []) day ledger
          t' = updateTimestep directive t
       in M.insert day t' ledger
    Nothing -> ledger

updateTimestep :: Directive  -> Timestep -> Timestep
updateTimestep directive ts@Timestep{..} = case directive of
    Cls c -> ts { _closings = c:_closings }
    Opn o -> ts { _openings = o:_openings}
    Bal b -> ts { _balances = b:_balances}
    Trn t -> ts { _transactions = t:_transactions}
    Prc p -> ts { _prices = p:_prices}
    _     -> ts

date :: Directive -> Maybe Day
date d =
  case d of
    Cls Close {_date}       -> Just _date
    Opn Open {_date}        -> Just _date
    Bal Balance {_date}     -> Just _date
    Trn Transaction {_date} -> Just _date
    Prc Price {_date}       -> Just _date
    _                       -> Nothing

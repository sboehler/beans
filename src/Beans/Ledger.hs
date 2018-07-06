module Beans.Ledger where

import           Beans.Data.Directives (Balance (..), Close (..),
                                        Directive (..), Open (..), Posting (..),
                                        Price (..), Transaction (..))
import qualified Beans.Data.Map        as M
import qualified Data.List             as L
import           Data.Time.Calendar    (Day)
import           Text.Regex.PCRE       ((=~))


data Timestep = Timestep
  {
    _openings     :: [Open]
  , _closings     :: [Close]
  , _balances     :: [Balance]
  , _transactions :: [Transaction]
  , _prices       :: [Price]
  } deriving (Show)

instance Monoid Timestep where
  mempty = Timestep [] [] [] [] []
  (Timestep o c b t p) `mappend` (Timestep o' c' b' t' p') =
    Timestep
      (o `mappend` o')
      (c `mappend` c')
      (b `mappend` b')
      (t `mappend` t')
      (p `mappend` p')

type Ledger = M.Map Day Timestep

buildLedger :: [Directive] -> Ledger
buildLedger = foldr updateLedger M.empty

updateLedger :: Directive -> Ledger -> Ledger
updateLedger d l =
  case date d of
    Just day -> M.insert day (updateTimestep d) l
    Nothing  -> l

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


-- filter a ledger 

filterLedger :: Bool -> String -> Ledger -> Ledger
filterLedger strict regex ledger =
  if strict
    then strictFilter <$> ledger
    else nonstrictFilter <$> ledger
  where
    nonstrictFilter t@Timestep {_transactions} =
      t {_transactions = L.filter (matchTransaction regex) _transactions}
    strictFilter t@Timestep {_transactions} =
      t
        { _transactions =
            filterPostings regex <$>
            L.filter (matchTransaction regex) _transactions
        }

filterPostings :: String -> Transaction -> Transaction
filterPostings regex trx@Transaction {_postings} =
  trx {_postings = L.filter (matchPosting regex) _postings}

matchTransaction :: String -> Transaction -> Bool
matchTransaction regex Transaction {_postings} =
  any (matchPosting regex) _postings
  

matchPosting :: String -> Posting -> Bool
matchPosting regex Posting {_account} = show _account =~ regex 

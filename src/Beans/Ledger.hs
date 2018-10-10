module Beans.Ledger
  ( Ledger
  , Timestep(..)
  , buildLedger
  , filterLedger
  )
where

import           Beans.Data.Directives                    ( Command(..)
                                                          , DatedCommand(..)
                                                          , Directive(..)
                                                          , Transaction(..)
                                                          )
import qualified Beans.Data.Map                as M
import qualified Data.List                     as L
import           Data.Time.Calendar                       ( Day )
import           Prelude                           hiding ( filter )
import           Text.Regex.PCRE                          ( (=~) )
import           Beans.Options                            ( Filter(..) )

data Timestep =
  Timestep Day
           [Command]
  deriving (Show)

type Ledger = [Timestep]

buildLedger :: [Directive] -> Ledger
buildLedger = build . L.sort . filter
 where
  filter d = [ c | (DatedCommandDirective c) <- d ]
  build = foldr add []
  add (DatedCommand d c) [] = [Timestep d [c]]
  add (DatedCommand d command) timesteps@(Timestep day commands : ts)
    | day == d  = Timestep day (command : commands) : ts
    | otherwise = Timestep d [command] : timesteps

-- filter a ledger
filterLedger :: Filter -> Ledger -> Ledger
filterLedger (StrictFilter regex) ledger = strictFilter <$> ledger
 where
  strictFilter (Timestep day commands) = Timestep
    day
    (filterPostings regex <$> L.filter (matchTransaction regex) commands)
filterLedger (Filter regex) ledger = nonstrictFilter <$> ledger
 where
  nonstrictFilter (Timestep day commands) =
    Timestep day (L.filter (matchTransaction regex) commands)
filterLedger NoFilter ledger = ledger

filterPostings :: String -> Command -> Command
filterPostings regex (TransactionCommand trx@Transaction { tPostings }) =
  TransactionCommand
    $ trx { tPostings = M.filterWithKey matchPosting tPostings }
  where matchPosting (a, _, _) _ = show a =~ regex
filterPostings _ command = command

matchTransaction :: String -> Command -> Bool
matchTransaction regex (TransactionCommand Transaction { tPostings }) =
  M.foldlWithKey g False tPostings
  where g b (a, _, _) _ = b || show a =~ regex
matchTransaction _ (BalanceCommand _) = False
matchTransaction _ _                  = True

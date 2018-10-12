module Beans.Ledger
  ( Ledger
  , buildLedger
  , filterLedger
  )
where

import           Beans.Data.Directives                    ( Command(..)
                                                          , Dated(..)
                                                          , Directive(..)
                                                          , Transaction(..)
                                                          )
import qualified Beans.Data.Map                as M
import qualified Data.List                     as L
import           Prelude                           hiding ( filter )
import           Text.Regex.PCRE                          ( (=~) )
import           Beans.Options                            ( Filter(..) )

type Ledger = [Dated Command]

buildLedger :: [Directive] -> Ledger
buildLedger = L.sort . filter
  where filter d = [ c | (DatedCommandDirective c) <- d ]

-- filter a ledger
filterLedger :: Filter -> Ledger -> Ledger
filterLedger (StrictFilter regex) =
  fmap (fmap (filterPostings regex)) . L.filter (matchCommand regex . undate)
filterLedger (Filter regex) = L.filter (matchCommand regex . undate)
filterLedger NoFilter       = id

filterPostings :: String -> Command -> Command
filterPostings regex (TransactionCommand trx@Transaction { tPostings }) =
  TransactionCommand
    $ trx { tPostings = M.filterWithKey matchPosting tPostings }
  where matchPosting (a, _, _) _ = show a =~ regex
filterPostings _ command = command

matchCommand :: String -> Command -> Bool
matchCommand regex (TransactionCommand Transaction { tPostings }) =
  M.foldlWithKey g False tPostings
  where g b (a, _, _) _ = b || show a =~ regex
matchCommand _ (BalanceCommand _) = False
matchCommand _ _                  = True

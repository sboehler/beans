module Beans.Ledger
  ( Ledger
  , build
  , filter
  )
where

import           Beans.Data.Directives                    ( Command(..)
                                                          , Dated(..)
                                                          , between
                                                          , Directive(..)
                                                          )
import qualified Beans.Data.Map                as M
import qualified Data.List                     as L
import           Prelude                           hiding ( filter )
import           Text.Regex.PCRE                          ( (=~) )
import           Beans.Options                            ( Filter(..) )

type Ledger = [Dated Command]

build :: [Directive] -> Ledger
build = L.sort . f where f d = [ c | (DatedCommandDirective c) <- d ]

filter :: Filter -> Ledger -> Ledger
filter (StrictFilter regex) =
  fmap (fmap (filterPostings regex)) . filter (Filter regex)
filter (Filter regex        ) = L.filter (matchCommand regex . undate)
filter (PeriodFilter from to) = L.filter (between from to)
filter NoFilter               = id

filterPostings :: String -> Command -> Command
filterPostings regex Transaction { tPostings, ..} = Transaction
  { tPostings = M.filterWithKey matchPosting tPostings
  , ..
  }
  where matchPosting (a, _, _) _ = show a =~ regex
filterPostings _ command = command

matchCommand :: String -> Command -> Bool
matchCommand regex Transaction { tPostings } = (any match . M.keys) tPostings
  where match (a, _, _) = show a =~ regex
matchCommand _ Balance {..} = False
matchCommand _ _            = True

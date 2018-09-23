module Beans.Ledger
  ( Ledger(..)
  , Timestep(..)
  , toList
  , buildLedger
  , filterLedger
  ) where

import           Beans.Data.Directives (Command (..), DatedCommand (..),
                                        Directive (..), Transaction (..))
import qualified Beans.Data.Map        as M
import qualified Data.List             as L
import           Data.Time.Calendar    (Day)
import           Prelude               hiding (filter)
import           Text.Regex.PCRE       ((=~))

data Timestep =
  Timestep Day
           [Command]
  deriving (Show)

newtype Ledger =
  Ledger [Timestep]
  deriving (Show)

toList :: Ledger -> [Timestep]
toList (Ledger l) = l

buildLedger :: [Directive] -> Ledger
buildLedger = Ledger . build . L.sort . filter
  where
    filter d = [c | (DatedCommandDirective c) <- d]
    build = foldr add []
    add (DatedCommand d c) [] = [Timestep d [c]]
    add (DatedCommand d command) timesteps@(Timestep day commands:ts)
      | day == d = Timestep day (command : commands) : ts
      | otherwise = Timestep d [command] : timesteps

-- filter a ledger
filterLedger :: Bool -> String -> Ledger -> Ledger
filterLedger strict regex ledger =
  if strict
    then Ledger $ strictFilter <$> toList ledger
    else Ledger $ nonstrictFilter <$> toList ledger
  where
    nonstrictFilter (Timestep day commands) =
      Timestep day (L.filter (matchTransaction regex) commands)
    strictFilter (Timestep day commands) =
      Timestep
        day
        (filterPostings regex <$> L.filter (matchTransaction regex) commands)

filterPostings :: String -> Command -> Command
filterPostings regex (TransactionCommand trx@Transaction {tPostings}) =
  TransactionCommand $ trx {tPostings = M.filterWithKey matchPosting tPostings}
    where
      matchPosting  (a, _, _) _ = show a =~ regex
filterPostings _ command = command

matchTransaction :: String -> Command -> Bool
matchTransaction regex (TransactionCommand Transaction {tPostings}) =
  M.foldlWithKey g False tPostings
    where
      g b (a, _, _) _ = b || show a =~ regex
matchTransaction _ (BalanceCommand _) = False
matchTransaction _ _ = True

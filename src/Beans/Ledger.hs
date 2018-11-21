module Beans.Ledger
  ( Ledger
  , build
  , fromFile
  , filter
  )
where


import           Beans.Model                              ( Command(..)
                                                          , Position(..)
                                                          , Dated(..)
                                                          , between
                                                          , Directive(..)
                                                          )
import qualified Beans.Data.Map                as M
import qualified Data.List                     as L
import           Prelude                           hiding ( filter )
import           Text.Regex.PCRE                          ( (=~) )
import           Beans.Options                            ( Filter(..) )
import           Beans.Parser                             ( parseFile )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO )

type Ledger = [Dated Command]

build :: [Directive] -> Ledger
build = L.sort . f where f d = [ c | (DatedCommandDirective c) <- d ]

fromFile :: (MonadIO m, MonadThrow m) => FilePath -> m Ledger
fromFile path = build <$> parseFile path

filter :: Filter -> Ledger -> Ledger
filter (StrictFilter regex) =
  fmap (fmap (filterPostings regex)) . filter (Filter regex)
filter (Filter regex        ) = L.filter (matchCommand regex . undate)
filter (PeriodFilter from to) = L.filter (between from to)
filter NoFilter               = id

filterPostings :: String -> Command -> Command
filterPostings regex Transaction { tPostings, ..} = Transaction
  { tPostings = M.filterKeys ((=~ regex) . show . pAccount) tPostings
  , ..
  }
filterPostings _ command = command

matchCommand :: String -> Command -> Bool
matchCommand regex Transaction { tPostings } = (any match . M.keys) tPostings
  where match = (=~ regex) . show . pAccount
matchCommand _ Balance {..} = False
matchCommand _ _            = True

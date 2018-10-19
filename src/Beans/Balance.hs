module Beans.Balance
  ( balanceCommand
  )
where

import           Beans.Table                              ( showTable )
import           Beans.Accounts                           ( checkLedger )
import           Beans.Report.Balance                     ( reportToTable
                                                          , createReport
                                                          )
import qualified Beans.Ledger                  as L
import           Beans.Options                            ( BalanceOptions(..) )
import           Beans.Parser                             ( parseFile )
import           Beans.Valuation                          ( valuateLedger )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          , ask
                                                          )
import           Prelude                           hiding ( filter )
import qualified Data.Text.IO                  as TIO

balanceCommand
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m ()
balanceCommand = parse >>= checkLedger >>= valuate >>= report
 where
  parse = L.build <$> (asks balOptJournal >>= parseFile)
  valuate ledger = asks balOptMarket >>= flip valuateLedger ledger
  report ledger =
    ask
      >>= flip createReport ledger
      >>= (liftIO . TIO.putStrLn . showTable . reportToTable)

module Beans.Journal
  ( journalCommand
  )
where

import           Beans.Options                            ( JournalOptions(..) )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , ask
                                                          , asks
                                                          )
import           Beans.Parser                             ( parseFile )
import qualified Beans.Ledger                  as L
import           Beans.Accounts                           ( checkLedger )
import           Beans.Valuation                          ( valuateLedger )
import           Beans.Report.Journal                     ( createReport
                                                          , reportToTable
                                                          )
import           Beans.Table                              ( showTable )
import qualified Data.Text.IO                  as TIO
import           Prelude                           hiding ( filter )


journalCommand
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m ()
journalCommand = parse >>= checkLedger >>= valuate >>= report
 where
  parse = L.build <$> (asks jrnOptJournal >>= parseFile)
  valuate ledger = asks jrnOptMarket >>= flip valuateLedger ledger
  report ledger =
    ask
      >>= flip createReport ledger
      >>= liftIO
      .   TIO.putStrLn
      .   showTable
      .   reportToTable

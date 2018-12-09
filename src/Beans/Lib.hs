module Beans.Lib
  ( run
  , Command(..)
  )
where

import           Beans.Report                   ( balance
                                                , journal
                                                , incomeStatement
                                                , balanceSheet
                                                )
import           Beans.Import                   ( importCommand )
import           Beans.Options                  ( Command(..) )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )

run :: (MonadIO m, MonadThrow m) => Command -> m ()
run (Balance         options) = balance options
run (IncomeStatement options) = incomeStatement options
run (BalanceSheet    options) = balanceSheet options
run (Import          options) = importCommand options
run (Journal         options) = journal options

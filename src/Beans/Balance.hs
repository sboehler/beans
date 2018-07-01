module Beans.Balance where

import           Beans.Data.Accounts    (Accounts, AccountsHistory, filter,
                                         lookupLE, minus)
import           Beans.Options          (Options (..))
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Time.Calendar     (Day)
import           Data.Time.LocalTime    (getZonedTime, localDay,
                                         zonedTimeToLocalTime)
import           Prelude                hiding (filter)

balanceReport :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
balanceReport accountsHistory = do
  to <- maybe (liftIO getDate) pure =<< asks optTo
  from <- asks optFrom
  let a1 = lookupLE to accountsHistory
      a0 = maybe mempty (`lookupLE` accountsHistory) from
  return $ filter (/= 0) $ a1 `minus` a0

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> getZonedTime

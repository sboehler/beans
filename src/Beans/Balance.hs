module Beans.Balance where

import Prelude hiding (filter)
import           Beans.Accounts           (AccountsHistory)
import           Beans.Data.Accounts      (Accounts, minus, filter)
import           Beans.Options            (Options (..))
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, asks)
import qualified Data.Map.Strict.Extended as M
import           Data.Time.Calendar       (Day)
import           Data.Time.LocalTime      (getZonedTime, localDay,
                                           zonedTimeToLocalTime)

balanceReport :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
balanceReport accountsHistory = do
  to <- maybe (liftIO getDate) pure =<< asks optTo
  from <- asks optFrom
  let a1 = M.lookupLessEqual to accountsHistory
      a0 = maybe mempty (`M.lookupLessEqual` accountsHistory) from
  return $ filter (/= 0) $ a1 `minus` a0

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> getZonedTime

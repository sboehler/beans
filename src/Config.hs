module Config where

import Data.Text (pack)
import Dhall
import Lens.Micro.Platform (makeFields)
import RIO
import System.Environment (getEnv)

data Config
  = Config
      { _configDatabaseName :: Text
      , _configDatabasePort :: Natural
      , _configDatabaseUser :: Text
      , _configDatabasePassword :: Text
      , _configDatabaseHost :: Text
      }
  deriving (Generic, Eq, Show)

instance Interpret Config

makeFields ''Config

getConfig :: MonadIO m => m Config
getConfig = do
  configFilePath <- liftIO $ getEnv "WEB_APP_CONFIG_FILE_PATH"
  liftIO $ (input auto (pack configFilePath) :: IO Config)

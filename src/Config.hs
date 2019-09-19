module Config where

import Data.Text (pack)
import Dhall
import Lens.Micro.Platform (makeFields)
import RIO
import System.Environment (lookupEnv)

data Config
  = Config
      { _configDatabaseName :: Text,
        _configDatabasePort :: Natural,
        _configDatabaseUser :: Text,
        _configDatabasePassword :: Text,
        _configDatabaseHost :: Text,
        _configAppPort :: Natural,
        _configAppKey :: Text
      }
  deriving (Generic, Eq, Show)

instance Interpret Config

makeFields ''Config

getConfig :: MonadIO m => m Config
getConfig = do
  configFilePath <- fromMaybe "./dev.config.dhall" <$> (liftIO $ lookupEnv "WEB_APP_CONFIG_FILE_PATH")
  liftIO $ (input auto (pack configFilePath) :: IO Config)

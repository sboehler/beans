module Beans.CLI.Import
  ( importOptions
  ) where

import           Beans.CLI.Common    (toReadM)
import           Beans.Data.Accounts (Account)
import           Beans.Options       (ImportOptions (..))
import qualified Beans.Parser        as P
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import           Options.Applicative

configFile :: Parser FilePath
configFile = argument
  str
  (metavar "<configuration file>" <> help "The configuration to use")

dataFile :: Parser FilePath
dataFile =
  argument str (metavar "<data file>" <> help "The data file to parse")

importer :: Parser Text
importer = strOption
  (metavar "<importer>" <> short 'i' <> help "Currently: only ch.postfinance")

account :: Parser Account
account = option (toReadM P.account)
                 (metavar "<account>" <> long "account" <> short 'a')

importOptions :: Parser ImportOptions
importOptions =
  ImportOptions <$> importer <*> configFile <*> account <*> dataFile

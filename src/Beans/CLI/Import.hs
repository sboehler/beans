module Beans.CLI.Import
  ( importOptions
  ) where

import           Beans.CLI.Common    (toReadM)
import           Beans.Data.Accounts (Account)
import           Beans.Options       (ImportOptions (..), Importer (..))
import qualified Beans.Parser        as P
import           Data.Semigroup      ((<>))
import           Options.Applicative

configFile :: Parser FilePath
configFile =
  argument
    str
    (metavar "<configuration file>" <> help "The configuration to use")

dataFile :: Parser FilePath
dataFile = argument str (metavar "<data file>" <> help "The data file to parse")

importer :: Parser Importer
importer =
  option
    parseImporter
    (metavar "<importer>" <> short 'i' <> help "The importer to use")
  where
    parseImporter =
      eitherReader $ \case
        "postfinance" -> Right Postfinance
        s -> Left $ "Invalid importer: " <> s

account :: Parser Account
account =
  option
    (toReadM P.account)
    (metavar "<account>" <> long "account" <> short 'a')

importOptions :: Parser ImportOptions
importOptions =
  ImportOptions <$> importer <*> configFile <*> account <*> dataFile

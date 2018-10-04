module Beans.Options where

import           Beans.Data.Accounts (Account, Commodity (..))
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

data Command
  = Balance BalanceOptions
  | Import ImportOptions
  deriving (Show)

data BalanceOptions = BalanceOptions
  { optJournal    :: FilePath
  , optMarket     :: Maybe Commodity
  , optLots       :: Bool
  , optFrom       :: Maybe Day
  , optTo         :: Maybe Day
  , optDepth      :: Maybe Int
  , optFilter     :: Filter
  , optReportType :: ReportType
  } deriving (Show)

data Filter = NoFilter | StrictFilter String | Filter String deriving (Eq, Show)

data ReportType
  = Hierarchical
  | Flat
  deriving (Eq, Show)

data ImportOptions = ImportOptions
  {
    optImporter :: Text
  , optConfig   :: FilePath
  , optAccount  :: Account
  , optData     :: FilePath
  } deriving (Show)

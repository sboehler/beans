module Beans.Options where

import           Beans.Data.Accounts (AccountName, CommodityName (..))
import           Data.Time.Calendar  (Day)

data Command
  = Balance BalanceOptions
  | Import ImportOptions
  deriving (Show)

data BalanceOptions = BalanceOptions
  { optJournal      :: FilePath
  , optMarket       :: Maybe CommodityName
  , optLots         :: Bool
  , optFrom         :: Maybe Day
  , optTo           :: Maybe Day
  , optDepth        :: Maybe Int
  , optFilter       :: Maybe String
  , optStrictFilter :: Bool
  , optReportType   :: ReportType
  } deriving (Show)

data ReportType
  = Hierarchical
  | Flat
  deriving (Eq, Show)

data ImportOptions = ImportOptions
  {
    optImporter :: Importer
  , optConfig   :: FilePath
  , optAccount  :: AccountName
  , optData     :: FilePath
  } deriving (Show)

data Importer =
  Postfinance
  deriving (Eq, Show)

module Beans.Options where

import           Beans.Data.Accounts (Account, Commodity (..))
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

data Command
  = Balance BalanceOptions
  | Import ImportOptions
  deriving (Show)

data BalanceOptions = BalanceOptions
  { balOptJournal    :: FilePath
  , balOptMarket     :: Valuation
  , balOptLots       :: Bool
  , balOptFrom       :: Maybe Day
  , balOptTo         :: Maybe Day
  , balOptDepth      :: Maybe Int
  , balOptFilter     :: Filter
  , balOptReportType :: ReportType
  } deriving (Show)

data Filter = NoFilter | StrictFilter String | Filter String deriving (Eq, Show)

data Valuation = NoValuation | AtMarket Commodity | AtCost Commodity deriving (Eq, Show)

data ReportType = Hierarchical | Flat  deriving (Eq, Show)

data ImportOptions = ImportOptions
  {
    impOptImporter :: Text
  , impOptConfig   :: FilePath
  , impOptAccount  :: Account
  , impOptData     :: FilePath
  } deriving (Show)

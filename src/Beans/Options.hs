module Beans.Options where

import           Beans.Model                              ( Account
                                                          , Commodity(..)
                                                          , Date
                                                          , Filter
                                                          )
import           Data.Text                                ( Text )


data Command
  = Balance BalanceOptions
  | IncomeStatement BalanceOptions
  | BalanceSheet BalanceOptions
  | Import ImportOptions
  | Journal JournalOptions
  deriving (Show)

data BalanceOptions = BalanceOptions
  { balOptJournal    :: FilePath
  , balOptMarket     :: Valuation
  , balOptLots       :: Bool
  , balOptFrom       :: Date
  , balOptTo         :: Date
  , balOptDepth      :: Maybe Int
  , balOptFilter     :: Filter
  , balOptReportType :: ReportType
  } deriving (Show)


data JournalOptions = JournalOptions
  { jrnOptJournal :: FilePath
  , jrnOptMarket  :: Valuation
  , jrnOptFrom    :: Date
  , jrnOptTo      :: Date
  , jrnOptRegex   :: Text
  } deriving (Show)

data Valuation = NoValuation | AtMarket Commodity Account | AtCost Commodity deriving (Eq, Show)

data ReportType = Hierarchical | Flat  deriving (Eq, Show)

data ImportOptions = ImportOptions
  {
    impOptImporter :: Text
  , impOptConfig   :: FilePath
  , impOptAccount  :: Account
  , impOptData     :: FilePath
  } deriving (Show)

module Beans.Options where

import           Beans.Data.Accounts                      ( Account
                                                          , Commodity(..)
                                                          , Date(..)
                                                          )
import           Data.Text                                ( Text )


data Command
  = Balance BalanceOptions
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
  { ledOptJournal :: FilePath
  , ledOptMarket  :: Valuation
  , ledOptFrom    :: Date
  , ledOptTo      :: Date
  , ledOptFilter  :: Filter
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

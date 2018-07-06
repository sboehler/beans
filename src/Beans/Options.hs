module Beans.Options where

import           Beans.Data.Accounts (CommodityName (..))
import           Data.Time.Calendar  (Day)

data Options = Options
  { optJournal :: FilePath
  , optMarket :: Maybe CommodityName
  , optLots :: Bool
  , optFrom :: Maybe Day
  , optTo :: Maybe Day
  , optDepth :: Maybe Int
  , optFilter :: Maybe String
  , optStrictFilter :: Bool
  , optReportType :: ReportType
  , optCommand :: Command
  } deriving (Show)


data ReportType
  = Hierarchical
  | Flat
  deriving (Eq, Show)

data Command =
  Balance
  deriving (Show)

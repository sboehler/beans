module Beans.Options where

import           Beans.Data.Accounts (CommodityName (..))
import           Data.Time.Calendar  (Day)

newtype Command =
  Balance BalanceOptions
  deriving (Show)


data BalanceOptions = BalanceOptions
  { optJournal :: FilePath
  , optMarket :: Maybe CommodityName
  , optLots :: Bool
  , optFrom :: Maybe Day
  , optTo :: Maybe Day
  , optDepth :: Maybe Int
  , optFilter :: Maybe String
  , optStrictFilter :: Bool
  , optReportType :: ReportType
  } deriving (Show)


data ReportType
  = Hierarchical
  | Flat
  deriving (Eq, Show)

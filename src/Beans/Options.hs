module Beans.Options where

import           Beans.AST          (CommodityName (..))
import           Data.Time.Calendar (Day)

data Options = Options
  { optJournal :: FilePath
  , optMarket  :: Maybe CommodityName
  , optLots    :: Bool
  , optFrom    :: Maybe Day
  , optTo      :: Maybe Day
  , optDepth   :: Maybe Int
  , optCommand :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)

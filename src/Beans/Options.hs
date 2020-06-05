module Beans.Options
  ( Command (..),
    BalanceFormat (..),
    BalanceOptions (..),
    FetchOptions (..),
    ImportOptions (..),
    InferOptions (..),
    Diffing (..),
  )
where

import Beans.Account (Account)
import qualified Beans.Command.Transcode as Transcode
import Beans.Commodity (Commodity)
import Beans.Date (Date, Interval)
import Beans.Filter (AccountFilter, Filter)
import Data.Text (Text)

data Command
  = Balance BalanceOptions
  | Fetch FetchOptions
  | Import ImportOptions
  | Infer InferOptions
  | Transcode Transcode.Options
  deriving (Show)

data InferOptions
  = InferOptions
      { infTrainingFile :: FilePath,
        infTargetFile :: FilePath
      }
  deriving (Show)

data Diffing = Diffing | NoDiffing deriving (Show)

type Collapse = [(AccountFilter, Int)]

data BalanceOptions
  = BalanceOptions
      { journal :: FilePath,
        valuation :: [Commodity],
        filter :: Filter,
        diffing :: Diffing,
        showCommodities :: Bool,
        balanceFormat :: BalanceFormat,
        fromDate :: Maybe Date,
        toDate :: Maybe Date,
        period :: Maybe Interval,
        percent :: Maybe AccountFilter,
        collapse :: Collapse
      }
  deriving (Show)

data BalanceFormat = Flat | Hierarchical deriving (Show)

data FetchOptions
  = FetchOptions
      { commodities :: Maybe [Commodity],
        configFile :: FilePath
      }
  deriving (Show)

data ImportOptions
  = ImportOptions
      { importer :: Text,
        account :: Account,
        inputFile :: FilePath
      }
  deriving (Show)

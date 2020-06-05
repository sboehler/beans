module Beans.Options
  ( Command (..),
    BalanceFormat (..),
    BalanceOptions (..),
    ImportOptions (..),
    Diffing (..),
  )
where

import Beans.Account (Account)
import qualified Beans.Command.Fetch as Fetch
import qualified Beans.Command.Infer as Infer
import qualified Beans.Command.Transcode as Transcode
import Beans.Commodity (Commodity)
import Beans.Date (Date, Interval)
import Beans.Filter (AccountFilter, Filter)
import Data.Text (Text)

data Command
  = Balance BalanceOptions
  | Fetch Fetch.Options
  | Import ImportOptions
  | Infer Infer.Options
  | Transcode Transcode.Options
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

data ImportOptions
  = ImportOptions
      { importer :: Text,
        account :: Account,
        inputFile :: FilePath
      }
  deriving (Show)

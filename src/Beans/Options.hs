module Beans.Options
  ( Command (..),
  )
where

import qualified Beans.Command.Balance as Balance
import qualified Beans.Command.Fetch as Fetch
import qualified Beans.Command.Import as Import
import qualified Beans.Command.Infer as Infer
import qualified Beans.Command.Transcode as Transcode

data Command
  = Balance Balance.Options
  | Fetch Fetch.Options
  | Import Import.Options
  | Infer Infer.Options
  | Transcode Transcode.Options
  deriving (Show)

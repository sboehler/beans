module Beans.Import.Common
  ( ImporterException (..),
    Config (..),
    Parser,
    BookingType (..),
    parseCommands,
    tags,
  )
where

import Beans.Account (Account)
import Beans.Command (Command)
import Beans.Transaction (Tag (..))
import Control.Exception (Exception)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.List as List
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

newtype ImporterException
  = ImporterException String
  deriving (Eq)

instance Show ImporterException where
  show (ImporterException s) = s

instance Exception ImporterException

data Config
  = Config
      { inputFile :: FilePath,
        account :: Account
      }

type Parser = ReaderT Config (Parsec Void Text)

parseCommands ::
  Config ->
  Parser [Command] ->
  Text ->
  Either ImporterException [Command]
parseCommands config parser input = do
  let parser' = runReaderT parser config
  case parse parser' (inputFile config) input of
    Left e -> (Left . ImporterException . errorBundlePretty) e
    Right d -> Right . List.sort $ d

data BookingType
  = MoneyTransfer
  | Interest
  | Fee
  | WithholdingTax
  | Dividend
  | Equity
  | CapitalGain
  | Deposit
  | Withdrawal
  deriving (Show, Read, Eq)

tags :: BookingType -> Tag
tags MoneyTransfer = Tag "#moneytransfer"
tags Interest = Tag "#interest"
tags Fee = Tag "#fee"
tags WithholdingTax = Tag "#withholdingtax"
tags Dividend = Tag "#dividend"
tags Equity = Tag "#equity"
tags CapitalGain = Tag "#capitalgain"
tags Deposit = Tag "#deposit"
tags Withdrawal = Tag "#withdrawal"

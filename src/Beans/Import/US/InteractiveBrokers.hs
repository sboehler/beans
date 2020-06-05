module Beans.Import.US.InteractiveBrokers
  ( parse,
  )
where

import qualified Beans.Account as Account
import Beans.Amount (Amount)
import Beans.Command (Command (CmdTransaction))
import Beans.Commodity (Commodity)
import qualified Beans.Date as Date
import qualified Beans.Import.Common as Common
import Beans.Lot (Lot (..))
import qualified Beans.Megaparsec as M
import Beans.Transaction (Posting (..), Transaction (..))
import Beans.ValAmount (ValAmount)
import Control.Monad (void)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.ByteString as B
import qualified Data.Char as Char
import Data.Group (invert)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

type Parser = StateT (Maybe Commodity) Common.Parser

parse :: Common.Config -> B.ByteString -> Either Common.ImporterException [Command]
parse config = Common.parseCommands config (State.evalStateT parseIBData Nothing) . Text.decodeLatin1

parseIBData :: Parser [Command]
parseIBData = catMaybes <$> M.many command

command :: Parser (Maybe Command)
command = M.choice [transaction, other]
  where
    transaction =
      Just
        <$> M.choice
          [ M.try depositWithdrawalOrFee,
            M.try trade,
            M.try dividendOrWithholdingTax
          ]
    other =
      Nothing
        <$ M.choice
          [ M.try baseCurrency,
            skipLine
          ]

depositWithdrawalOrFee :: Parser Command
depositWithdrawalOrFee = do
  t <-
    M.choice
      [ Common.MoneyTransfer <$ constField "Deposits & Withdrawals" <* constField "Data",
        Common.Interest <$ constField "Interest" <* constField "Data",
        Common.Fee <$ constField "Fees" <* constField "Data" <* constField "Other Fees"
      ]
  currency <- commodityField
  date <- dateField
  description <- textField
  amount <- amountField
  account <- Reader.asks Common.account
  let bookings =
        [ Posting account currency Nothing amount Nothing,
          Posting Account.unknown currency Nothing (invert amount) (Just $ Common.tags t)
        ]
  return $ CmdTransaction $
    Transaction
      date
      (Text.unwords [Text.pack . show $ t, "-", description])
      []
      bookings

trade :: Parser Command
trade = do
  _ <- constField "Trades" >> constField "Data" >> constField "Order"
  description <- textField
  currency <- commodityField
  symbol <- commodityField
  date <- dateField
  amount <- amountField
  price <- priceField
  _ <- skipField
  purchaseAmount <- amountField
  feeAmount <- invert <$> amountField <* skipRestOfLine
  account <- Reader.asks Common.account
  feeCurrency <- State.get >>= \case
    Nothing -> fail "No base currency"
    Just b -> pure $ if description == "Forex" then b else currency
  let lot = Lot price currency date Nothing
      bookings =
        [ Posting account symbol (Just lot) amount Nothing,
          Posting Account.unknown symbol (Just lot) (invert amount) (Just $ Common.tags Common.Equity),
          Posting account currency Nothing purchaseAmount Nothing,
          Posting account feeCurrency Nothing (invert feeAmount) Nothing,
          Posting Account.unknown feeCurrency Nothing feeAmount (Just $ Common.tags Common.Fee),
          Posting Account.unknown currency Nothing (invert purchaseAmount) (Just $ Common.tags Common.Equity)
        ]
  return $ CmdTransaction $ Transaction date description [] bookings

dividendOrWithholdingTax :: Parser Command
dividendOrWithholdingTax = do
  t <- field $ M.choice [Common.WithholdingTax <$ M.string "Withholding Tax", Common.Dividend <$ M.string "Dividends"]
  currency <- constField "Data" >> commodityField
  date <- dateField
  (symbol, isin, rest) <- field $ do
    symbol <- M.parseCommodity <* M.space
    isin <- M.char '(' >> M.takeWhile1P (Just "ISIN") Char.isAlphaNum <* M.char ')' <* M.space
    rest <- M.takeWhile1P Nothing (const True)
    pure (symbol, isin, rest)
  amount <- amountField
  account <- Reader.asks Common.account
  let desc = Text.unwords [Text.pack . show $ t, Text.pack . show $ symbol, isin, rest]
      bookings =
        [ Posting account currency Nothing amount Nothing,
          Posting account symbol Nothing mempty Nothing,
          Posting Account.unknown currency Nothing (invert amount) (Just $ Common.tags t)
        ]
  return $ CmdTransaction $ Transaction date desc [] bookings

baseCurrency :: Parser ()
baseCurrency = do
  _ <- field $ M.string "Account Information"
  _ <- field $ M.string "Data"
  _ <- field $ M.string "Base Currency"
  field $ Just <$> M.parseCommodity >>= State.put

textField :: Parser Text
textField = field $ M.takeWhile1P Nothing (/= ',')

constField :: Text -> Parser Text
constField = field . M.string

dateField :: Parser Date.Date
dateField = field M.parseISODate

commodityField :: Parser Commodity
commodityField = field M.parseCommodity

amountField :: Parser ValAmount
amountField = field $ M.preprocess filterCommas p
  where
    filterCommas = Text.filter (/= ',')
    p = M.parseValAmount $ pure ()

priceField :: Parser Amount
priceField = field $ M.preprocess filterCommas p
  where
    filterCommas = Text.filter (/= ',')
    p = M.parseAmount $ pure ()

skipLine :: Parser ()
skipLine = void $ M.skipManyTill M.anySingle M.eol

skipField :: Parser ()
skipField = void $ M.skipManyTill M.anySingle separator

skipRestOfLine :: Parser ()
skipRestOfLine = void $ M.skipManyTill M.anySingle M.eol

separator :: Parser ()
separator = void comma

comma :: Parser Char
comma = M.char ','

field :: Parser a -> Parser a
field = M.subparse (Text.pack <$> M.choice [quoted, unquoted])
  where
    quote = M.char '"'
    quoted = quote >> M.manyTill M.anySingle (quote >> separator)
    unquoted = M.manyTill M.anySingle separator

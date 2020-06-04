module Beans.Import.CH.Swissquote
  ( parse,
  )
where

import qualified Beans.Account as Account
import Beans.Command (Command (CmdTransaction))
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import qualified Beans.Import.Common as Common
import qualified Beans.Megaparsec as M
import Beans.Transaction (Posting (..), Transaction (..))
import Beans.ValAmount (ValAmount)
import Control.Monad (replicateM_, unless, void)
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as B
import Data.Group (invert)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Common.Parser

parse :: Common.Config -> B.ByteString -> Either Common.ImporterException [Command]
parse config = Common.parseCommands config parser . Text.decodeLatin1

parser :: Parser [Command]
parser = header >> M.some line

header :: Parser ()
header = do
  _ <- string "Datum"
  _ <- string "Auftrag #"
  _ <- string "Transaktionen"
  _ <- string "Symbol"
  _ <- string "Name"
  _ <- string "ISIN"
  _ <- string "Anzahl"
  _ <- string "Stückpreis"
  _ <- string "Kosten"
  _ <- string "Aufgelaufene Zinsen"
  _ <- string "Nettobetrag"
  _ <- string "Saldo"
  _ <- string "Währung"
  pure ()

line :: Parser Command
line =
  M.choice
    [ M.try dividend,
      M.try interest,
      M.try capitalGain,
      M.try forex,
      M.try trade,
      M.try fees,
      deposit
    ]

dividend :: Parser Command
dividend = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  dividendStr <- string "Dividende"
  sym <- commodityField
  symName <- textField
  isin <- textField
  _ <- amountField
  gross <- amountField
  tax <- amountField
  _ <- amountField
  net <- amountField
  _ <- amountField
  currency <- commodityField
  let desc = Text.unwords [dividendStr, Text.pack . show $ sym, symName, isin]
      bookings =
        [ Posting account currency Nothing net Nothing,
          Posting Account.unknown sym Nothing mempty (Just $ Common.tags Common.Dividend),
          Posting Account.unknown currency Nothing (invert gross) (Just $ Common.tags Common.Dividend),
          Posting Account.unknown currency Nothing tax (Just $ Common.tags Common.WithholdingTax)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

capitalGain :: Parser Command
capitalGain = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  dividendStr <- M.choice [string "Capital Gain", string "Kapitalrückzahlung"]
  sym <- commodityField
  symName <- textField
  isin <- textField
  _ <- amountField
  gross <- amountField
  _ <- amountField
  _ <- amountField
  net <- amountField
  _ <- amountField
  currency <- commodityField
  let desc = Text.unwords [dividendStr, Text.pack . show $ sym, symName, isin]
      bookings =
        [ Posting account currency Nothing net Nothing,
          Posting Account.unknown sym Nothing mempty (Just $ Common.tags Common.CapitalGain),
          Posting Account.unknown currency Nothing (invert gross) (Just $ Common.tags Common.CapitalGain)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

interest :: Parser Command
interest = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  interestStr <- string "Zins"
  replicateM_ 3 ignoreField
  _ <- amountField
  gross <- amountField
  tax <- amountField
  _ <- amountField
  net <- amountField
  _ <- amountField
  currency <- commodityField
  let desc = Text.unwords [interestStr, Text.pack . show $ currency]
      bookings =
        [ Posting account currency Nothing net Nothing,
          Posting Account.unknown currency Nothing (invert gross) (Just $ Common.tags Common.Interest),
          Posting Account.unknown currency Nothing tax (Just $ Common.tags Common.WithholdingTax)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

forex :: Parser Command
forex = do
  account <- Reader.asks Common.account
  (date, desc1, amount1, currency1) <- leg
  (date', desc2, amount2, currency2) <- leg
  let desc =
        Text.unwords
          [ desc1,
            Text.pack . show $ currency1,
            desc2,
            Text.pack . show $ currency2
          ]
      bookings =
        [ Posting account currency1 Nothing amount1 Nothing,
          Posting account currency2 Nothing amount2 Nothing,
          Posting Account.unknown currency1 Nothing (invert amount1) (Just $ Common.tags Common.Equity),
          Posting Account.unknown currency2 Nothing (invert amount2) (Just $ Common.tags Common.Equity)
        ]
  unless (date == date') $ fail "Dates do not match"
  pure $ CmdTransaction $ Transaction date desc [] bookings
  where
    leg = do
      date <- dateField
      _ <- ignoreField
      desc <-
        M.choice
          [ string "Forex-Gutschrift",
            string "Forex-Belastung",
            string "Fx-Belastung Comp.",
            string "Fx-Gutschrift Comp."
          ]
      replicateM_ 7 ignoreField
      net <- amountField
      _ <- amountField
      currency <- commodityField
      pure (date, desc, net, currency)

trade :: Parser Command
trade = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  description <- M.choice [string "Kauf", string "Verkauf"]
  sym <- commodityField
  symName <- textField
  isin <- textField
  nbr <- amountField
  purchaseAmount <- amountField
  feeAmount <- amountField
  _ <- amountField
  totalAmount <- amountField
  _ <- amountField
  currency <- commodityField
  let desc =
        Text.unwords
          [ description,
            Text.pack . show $ sym,
            symName,
            isin,
            Text.pack . show $ purchaseAmount,
            Text.pack . show $ currency
          ]
      bookings =
        [ Posting account sym Nothing (if description == "Kauf" then nbr else invert nbr) Nothing,
          Posting account currency Nothing totalAmount Nothing,
          Posting Account.unknown currency Nothing feeAmount (Just $ Common.tags Common.Fee),
          Posting Account.unknown sym Nothing (if description == "Kauf" then invert nbr else nbr) (Just $ Common.tags Common.Equity),
          Posting Account.unknown currency Nothing (invert (totalAmount <> feeAmount)) (Just $ Common.tags Common.Equity)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

deposit :: Parser Command
deposit = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  description <- M.choice [string "Vergütung", string "Auszahlung", string "Einzahlung"]
  replicateM_ 7 ignoreField
  amount <- amountField
  _ <- amountField
  currency <- commodityField
  let desc =
        Text.unwords
          [ description,
            Text.pack . show $ amount,
            Text.pack . show $ currency
          ]
      bookings =
        [ Posting account currency Nothing amount Nothing,
          Posting Account.unknown currency Nothing (invert amount) (Just $ Common.tags Common.Deposit)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

fees :: Parser Command
fees = do
  account <- Reader.asks Common.account
  date <- dateField
  _ <- ignoreField
  description <- M.choice [string "Depotgebühren", string "Berichtigung Börsengeb."]
  replicateM_ 7 ignoreField
  amount <- amountField
  _ <- amountField
  currency <- commodityField
  let desc =
        Text.unwords
          [ description,
            Text.pack . show $ amount,
            Text.pack . show $ currency
          ]
      bookings =
        [ Posting account currency Nothing amount Nothing,
          Posting Account.unknown currency Nothing (invert amount) (Just $ Common.tags Common.Fee)
        ]
  pure $ CmdTransaction $ Transaction date desc [] bookings

commodityField :: Parser Commodity
commodityField = field M.parseCommodity

amountField :: Parser ValAmount
amountField = field $ M.preprocess (Text.filter (/= '\'')) $ M.parseValAmount M.space

textField :: Parser Text
textField = field $ M.takeWhile1P Nothing (/= ';')

dateField :: Parser Date
dateField = do
  d <- M.parseFormattedDate "%d-%m-%Y" (Text.unpack <$> M.takeP (Just "DD-MM-YYYY") 10)
  _ <- ignoreField
  pure d

string :: Text -> Parser Text
string = field . M.string

ignoreField :: Parser ()
ignoreField = void $ M.skipManyTill M.anySingle separator

field :: Parser a -> Parser a
field = L.lexeme separator

separator :: Parser ()
separator = void $ M.choice [M.string ";", M.eol]

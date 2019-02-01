module Beans.Import.US.InteractiveBrokers
  ( name
  , parse
  )
where

import qualified Beans.Data.Map                as M
import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , askAccount
                                                , parseCommands
                                                )
import qualified Beans.Import.Common           as Common
import           Beans.Megaparsec               ( alphaNumChar
                                                , char
                                                , choice
                                                , anySingle
                                                , parseAmount
                                                , parseISODate
                                                , space
                                                , string
                                                , eol
                                                , skipManyTill
                                                , manyTill
                                                , subparse
                                                , preprocess
                                                , takeWhile1P
                                                , some
                                                , many
                                                , try
                                                , (<|>)
                                                )
import           Beans.Model                    ( Commodity(..)
                                                , Dated(Dated)
                                                , Flag(Complete)
                                                , Command(CmdTransaction)
                                                , Transaction(..)
                                                , Date
                                                , Lot(Lot)
                                                , Position(Position)
                                                , Amount
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import qualified Data.ByteString               as B
import           Data.Char                      ( isAlphaNum )
import           Data.Group                     ( invert )
import qualified Data.List                     as List
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                , unwords
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeLatin1 )
import           Prelude                 hiding ( unwords )

type Parser = StateT (Maybe Commodity) Common.Parser

name :: Text
name = "us.interactivebrokers"

parse
  :: Config -> B.ByteString -> Either Common.ImporterException [Dated Command]
parse config bytes =
  let parser = evalStateT parseIBData Nothing
  in  parseCommands config parser (decodeLatin1 bytes)

data AccountType = MoneyTransfer | Interest | Fee | WithholdingTax | Dividend
  deriving (Show, Read, Eq)

parseIBData :: Parser [Dated Command]
parseIBData = List.sort . catMaybes <$> many line

line :: Parser (Maybe (Dated Command))
line =
  Just
    <$> (   try depositWithdrawalOrFee
        <|> try trade
        <|> try dividendOrWithholdingTax
        )
    <|> (skipLine >> pure Nothing)

depositWithdrawalOrFee :: Parser (Dated Command)
depositWithdrawalOrFee = do
  t <-
    try (MoneyTransfer <$ cField "Deposits & Withdrawals" <* cField "Data")
    <|> try (Interest <$ cField "Interest" <* cField "Data")
    <|> try (Fee <$ cField "Fees" <* (cField "Data" >> cField "Other Fees"))
  currency    <- commodityField
  date        <- dateField
  description <- textField
  amount      <- parseAmount (pure ()) <* skipRestOfLine
  account     <- asks _configAccount
  other       <- askAccount
    $ Context date (pack . show $ t) description amount currency name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , (Position other currency Nothing  , M.singleton currency (-amount))
        ]
  return $ Dated date $ CmdTransaction $ Transaction
    Complete
    (unwords [pack . show $ t, "-" :: Text, description])
    []
    bookings

trade :: Parser (Dated Command)
trade = do
  description <- cField "Trades" >> cField "Data" >> cField "Order" >> textField
  currency       <- commodityField
  symbol         <- commodityField
  date           <- dateField
  amount         <- amountField
  price          <- amountField <* skipField
  purchaseAmount <- amountField
  feeAmount      <- invert <$> amountField <* skipRestOfLine
  account        <- asks _configAccount
  feeAccount     <- askAccount
    $ Context date (pack . show $ Fee) description feeAmount currency name
  let
    -- TODO: Determine reference currency
    feeCommodity =
      if description == "Forex" then Commodity "Unknown" else currency
    lot      = Lot price currency date Nothing
    bookings = M.fromListM
      [ (Position account symbol (Just lot), M.singleton symbol amount)
      , (Position account currency Nothing, M.singleton currency purchaseAmount)
      , ( Position account currency Nothing
        , M.singleton feeCommodity (-feeAmount)
        )
      , ( Position feeAccount currency Nothing
        , M.singleton feeCommodity feeAmount
        )
      ]
  return $ Dated date $ CmdTransaction $ Transaction Complete
                                                     description
                                                     []
                                                     bookings

dividendOrWithholdingTax :: Parser (Dated Command)
dividendOrWithholdingTax = do
  t <- field $ choice
    [WithholdingTax <$ string "Withholding Tax", Dividend <$ string "Dividends"]
  currency <- cField "Data" >> commodityField
  date     <- dateField
  symbol   <-
    Commodity <$> takeWhile1P (Just "Commodity name") isAlphaNum <* space
  isin <-
    char '(' >> takeWhile1P (Just "ISIN") isAlphaNum <* (char ')' >> space)
  description <- field $ takeWhile1P Nothing (const True)
  amount      <- parseAmount (pure ()) <* skipRestOfLine
  account     <- asks _configAccount
  let desc = unwords [pack . show $ t, pack . show $ symbol, isin, description]
  dividendAccount <- askAccount
    $ Context date (pack . show $ t) desc amount currency name
  let
    bookings = M.fromListM
      [ (Position account currency Nothing, M.singleton currency amount)
      , (Position account symbol Nothing  , M.singleton symbol 0)
      , ( Position dividendAccount currency Nothing
        , M.singleton currency (-amount)
        )
      ]
  return $ Dated date $ CmdTransaction $ Transaction Complete desc [] bookings

textField :: Parser Text
textField = field $ takeWhile1P Nothing (/= ',')

cField :: Text -> Parser Text
cField = field . string

dateField :: Parser Date
dateField = field parseISODate

commodityField :: Parser Commodity
commodityField =
  Commodity . pack <$> (some alphaNumChar <* skipManyTill anySingle separator)

amountField :: Parser Amount
amountField = field $ preprocess filterCommas p
 where
  filterCommas = Text.filter (/= ',')
  p            = parseAmount $ pure ()

skipLine :: Parser ()
skipLine = void $ skipManyTill anySingle eol

skipField :: Parser ()
skipField = void $ skipManyTill anySingle separator

skipRestOfLine :: Parser ()
skipRestOfLine = void $ skipManyTill anySingle eol

separator :: Parser ()
separator = void comma

comma :: Parser Char
comma = char ','

field :: Parser a -> Parser a
field = subparse (quotedField <|> unquotedField)
 where
  quote         = char '"'
  quotedField   = pack <$> (quote >> manyTill anySingle (quote >> separator))
  unquotedField = pack <$> manyTill anySingle separator

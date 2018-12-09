module Beans.Import.US.InteractiveBrokers
  ( name
  , parse
  )
where

import           Prelude                 hiding ( unwords )
import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text
                                                , pack
                                                , unwords
                                                , toLower
                                                )
import           Data.Monoid                    ( Sum(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )

import           Beans.Import.Common            ( Context(..)
                                                , Parser
                                                , askAccount
                                                , parseLatin1
                                                , Config(..)
                                                )
import qualified Beans.Data.Map                as M
import qualified Text.Megaparsec.Char.Lexer    as L
import           Beans.Model                    ( Commodity(..)
                                                , Dated(Dated)
                                                , Flag(Complete)
                                                , Command(CmdTransaction)
                                                , Transaction(..)
                                                , Date
                                                , fromGreg
                                                , Lot(Lot)
                                                , Position(Position)
                                                , Amount
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , anyChar
                                                , space
                                                , string
                                                , digitChar
                                                , eol
                                                )
import           Text.Megaparsec                ( count
                                                , skipManyTill
                                                , takeWhile1P
                                                , some
                                                , many
                                                , try
                                                , (<|>)
                                                )
import           Data.Maybe                     ( catMaybes )
import           Control.Monad.Reader           ( asks
                                                , MonadReader
                                                )

name :: Text
name = "us.interactivebrokers"

parse :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parse = parseLatin1 parseIBData


parseIBData :: Parser [Dated Command]
parseIBData = catMaybes <$> many line

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
    try (cField "Deposits & Withdrawals" <* cField "Data")
    <|> try (cField "Interest" <* cField "Data")
    <|> try (cField "Fees" <* (cField "Data" >> cField "Other Fees"))
  currency    <- commodityField
  date        <- dateField
  description <- textField
  amount      <- amountField <* skipRestOfLine
  account     <- asks _configAccount
  other       <- askAccount
    $ Context date (toLower t) description amount currency name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , (Position other currency Nothing  , M.singleton currency (-amount))
        ]
  return $ Dated date $ CmdTransaction $ Transaction
    Complete
    (unwords [t, "-" :: Text, description])
    []
    bookings

trade :: Parser (Dated Command)
trade = do
  category <- cField "Trades" >> cField "Data" >> cField "Order" >> textField
  currency              <- commodityField
  symbol                <- commodityField
  date                  <- dateTimeField
  amount                <- amountField
  price                 <- amountField <* skipField
  purchas_contextAmount <- amountField
  fe_contextAmount      <- amountField <* skipRestOfLine
  account               <- asks _configAccount
  feeAccount            <- askAccount
    $ Context date "fee" category fe_contextAmount currency name
  let lot      = Lot price currency date Nothing
      bookings = M.fromListM
        [ (Position account symbol (Just lot), M.singleton symbol amount)
        , ( Position account currency Nothing
          , M.singleton currency purchas_contextAmount
          )
        , ( Position feeAccount currency Nothing
          , M.singleton currency fe_contextAmount
          )
        ]
  return $ Dated date $ CmdTransaction $ Transaction Complete
                                                     category
                                                     []
                                                     bookings

dividendOrWithholdingTax :: Parser (Dated Command)
dividendOrWithholdingTax = do
  t        <- cField "Dividends" <|> cField "Withholding Tax"
  currency <- cField "Data" >> commodityField
  date     <- dateField
  --description     <- textField
  symbol   <-
    Commodity <$> takeWhile1P (Just "Commodity name") isAlphaNum <* space
  isin <-
    char '(' >> takeWhile1P (Just "ISIN") isAlphaNum <* (char ')' >> space)
  description <- field $ takeWhile1P Nothing (/= ',')
  amount      <- amountField
  account     <- asks _configAccount
  let desc = unwords [t, (pack . show) symbol, isin, description]
  dividendAccount <- askAccount
    $ Context date (toLower t) desc amount currency name
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
dateField = field (fromGreg <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
 where
  dash = char '-'
  int n = read <$> count n digitChar

dateTimeField :: Parser Date
dateTimeField =
  field
    $  quote
    >> (fromGreg <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
    <* skipManyTill anyChar quote
 where
  dash  = char '-'
  quote = char '"'
  int n = read <$> count n digitChar

commodityField :: Parser Commodity
commodityField = field $ Commodity . pack <$> some alphaNumChar

amountField :: Parser Amount
amountField = field $ Sum <$> L.signed (pure ()) L.scientific

skipLine :: Parser ()
skipLine = void $ skipManyTill anyChar eol

skipField :: Parser ()
skipField = void $ skipManyTill anyChar separator

skipRestOfLine :: Parser ()
skipRestOfLine = void $ skipManyTill anyChar eol

separator :: Parser ()
separator = void comma

comma :: Parser Char
comma = char ','

field :: Parser a -> Parser a
field = L.lexeme separator

module Beans.Import.US.InteractiveBrokers
  ( name
  , parseEntries
  )
where

import qualified Data.Text                     as T
import           Data.Text                                ( Text
                                                          , pack
                                                          )
import           Data.Monoid                              ( Sum(..) )
import           Beans.Data.Directives                    ( Dated(Dated)
                                                          , Flag(Complete)
                                                          , Command(Transaction)
                                                          )
import           Control.Monad                            ( void )
import           Control.Monad.Catch                      ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )

import           Beans.Import.Common                      ( Entry(..)
                                                          , ImporterException(..)
                                                          , Config(..)
                                                          )
import qualified Data.ByteString               as B
import           Data.Text.Encoding                       ( decodeLatin1 )

import qualified Beans.Data.Map                as M
import qualified Text.Megaparsec.Char.Lexer    as L
import           Beans.Data.Accounts                      ( Commodity(..)
                                                          , Date(Date)
                                                          , Lot(Lot)
                                                          , Account
                                                          , Position(Position)
                                                          , Amount
                                                          )
import           Text.Megaparsec.Char                     ( alphaNumChar
                                                          , char
                                                          , anyChar
                                                          , string
                                                          , digitChar
                                                          , eol
                                                          )
import           Control.Exception                        ( Exception )
import           Text.Megaparsec                          ( Parsec
                                                          , ShowErrorComponent
                                                          , showErrorComponent
                                                          , customFailure
                                                          , count
                                                          , skipManyTill
                                                          , parse
                                                          , parseErrorPretty
                                                          , takeWhile1P
                                                          , some
                                                          , many
                                                          , try
                                                          , (<|>)
                                                          )
import           Data.Maybe                               ( catMaybes )
import           Data.Time.Calendar                       ( fromGregorian )
import           Control.Monad.Reader                     ( ReaderT
                                                          , asks
                                                          , MonadReader
                                                          , ask
                                                          , runReaderT
                                                          )



type Parser = ReaderT Config (Parsec ParserException Text)

-- parser exception
newtype ParserException =
  AccountNotFound Text
  deriving (Eq, Show, Ord)

instance Exception ParserException

instance ShowErrorComponent ParserException where
  showErrorComponent (AccountNotFound t) =
    "Account not found: " ++ show t


name :: Text
name = "us.interactivebrokers"

parseEntries
  :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parseEntries = do
  c@Config { cFile } <- ask
  source             <- liftIO $ decodeLatin1 <$> B.readFile cFile
  case parse (runReaderT (many line) c) mempty source of
    Left  e -> (throwM . ImporterException . parseErrorPretty) e
    Right d -> catMaybes <$> return d


line :: Parser (Maybe (Dated Command))
line =
  (Just <$> try depositOrWithdrawal)
    <|> (Just <$> try trade)
    <|> (Just <$> try dividend)
    <|> (Just <$> try withholdingTax)
    <|> (skipLine >> pure Nothing)

askAccount :: Entry -> Parser Account
askAccount entry = do
  evaluator <- asks cEvaluator
  case evaluator entry of
    Just account -> return account
    Nothing      -> customFailure $ AccountNotFound $ pack . show $ entry


depositOrWithdrawal :: Parser (Dated Command)
depositOrWithdrawal = do
  currency <-
    constantField "Deposits & Withdrawals"
    >> constantField "Data"
    >> commodityField
  date        <- dateField
  description <- textField
  amount      <- amountField <* skipRestOfLine
  account     <- asks cAccount
  other <- askAccount $ Entry date
                              (if amount > 0 then "deposit" else "withdrawal")
                              description
                              (-amount)
                              currency
                              name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , (Position other currency Nothing  , M.singleton currency (-amount))
        ]
  return $ Dated date $ Transaction Complete description [] bookings

trade :: Parser (Dated Command)
trade = do
  category <-
    constantField "Trades"
    >> constantField "Data"
    >> constantField "Order"
    >> textField
  currency       <- commodityField
  symbol         <- commodityField
  date           <- dateTimeField
  amount         <- amountField
  price          <- amountField <* skipField
  purchaseAmount <- amountField
  feeAmount      <- amountField <* skipRestOfLine
  account        <- asks cAccount
  feeAccount <- askAccount $ Entry date "fee" category feeAmount currency name
  let
    lot      = Lot price currency date Nothing
    bookings = M.fromListM
      [ (Position account symbol (Just lot), M.singleton symbol amount)
      , (Position account currency Nothing, M.singleton currency purchaseAmount)
      , (Position feeAccount currency Nothing, M.singleton currency feeAmount)
      ]
  return $ Dated date $ Transaction Complete category [] bookings

dividend :: Parser (Dated Command)
dividend = do
  currency <-
    constantField "Dividends" >> constantField "Data" >> commodityField
  date            <- dateField
  description     <- textField
  amount          <- amountField
  account         <- asks cAccount
  dividendAccount <- askAccount
    $ Entry date "dividend" description amount currency name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , ( Position dividendAccount currency Nothing
          , M.singleton currency (-amount)
          )
        ]
  return $ Dated date $ Transaction Complete description [] bookings

withholdingTax :: Parser (Dated Command)
withholdingTax = do
  currency <-
    constantField "Withholding Tax" >> constantField "Data" >> commodityField
  date                  <- dateField
  description           <- textField
  amount                <- amountField
  account               <- asks cAccount
  withholdingTaxAccount <- askAccount
    $ Entry date "withholding tax" description amount currency name
  let bookings = M.fromListM
        [ (Position account currency Nothing, M.singleton currency amount)
        , ( Position withholdingTaxAccount currency Nothing
          , M.singleton currency (-amount)
          )
        ]
  return $ Dated date $ Transaction Complete description [] bookings


textField :: Parser Text
textField = field $ takeWhile1P Nothing (/= ',')

constantField :: Text -> Parser Text
constantField = field . string

dateField :: Parser Date
dateField =
  field
    $   Date
    <$> (fromGregorian <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
 where
  dash = char '-'
  int n = read <$> count n digitChar

dateTimeField :: Parser Date
dateTimeField =
  field
    $  quote
    >> (   Date
       <$> (fromGregorian <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
       )
    <* skipManyTill anyChar quote
 where
  dash  = char '-'
  quote = char '"'
  int n = read <$> count n digitChar



commodityField :: Parser Commodity
commodityField = field $ Commodity . T.pack <$> some alphaNumChar

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

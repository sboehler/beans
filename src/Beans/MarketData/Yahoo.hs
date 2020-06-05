module Beans.MarketData.Yahoo
  ( TimeSeries (..),
    TimeSeriesEntry (..),
    Symbol (Symbol),
    getDailySeries,
  )
where

import Beans.Date (Date)
import qualified Beans.Megaparsec as M
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Fixed (div')
import Data.Maybe (catMaybes)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time.Clock as Clock
import Data.Time.Clock (addUTCTime)
import qualified Data.Time.Clock.POSIX as POSIX
import Data.Void (Void)
import qualified Network.HTTP.Simple as HTTP
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- High level methods
--------------------------------------------------------------------------------

getDailySeries :: (MonadThrow m, MonadIO m) => Symbol -> m TimeSeries
getDailySeries s = do
  d1 <- liftIO Clock.getCurrentTime
  let r =
        TimeSeriesRequest
          s
          ((`div'` 1) . POSIX.utcTimeToPOSIXSeconds $ addUTCTime (-3600 * 365 * 24) d1)
          ((`div'` 1) . POSIX.utcTimeToPOSIXSeconds $ d1)
  text <- HTTP.getResponseBody <$> request r
  TimeSeries <$> parseSource parseCSV "" text

--------------------------------------------------------------------------------
-- API Implementation
--------------------------------------------------------------------------------

newtype Symbol = Symbol String

instance Show Symbol where
  show (Symbol s) = s

data Request
  = TimeSeriesRequest Symbol Int Int

baseURL :: String
baseURL = "https://query1.finance.yahoo.com/v7/finance/download/"

createURL :: Request -> HTTP.Request
createURL (TimeSeriesRequest (Symbol symbol) period1 period2) =
  HTTP.setRequestQueryString params
    . HTTP.parseRequest_
    $ url
  where
    url = baseURL <> symbol
    params =
      fmap
        (fmap (Just . BS.pack))
        [ ("period1", show period1),
          ("period2", show period2),
          ("events", "history"),
          ("interval", "1d")
        ]

request :: (MonadIO m) => Request -> m (HTTP.Response Text)
request r = fmap Encoding.decodeUtf8 <$> HTTP.httpBS (createURL r)

--------------------------------------------------------------------------------
-- Daily Series
--------------------------------------------------------------------------------

newtype TimeSeries
  = TimeSeries
      { entries :: [TimeSeriesEntry]
      }
  deriving (Show)

data TimeSeriesEntry
  = TimeSeriesEntry
      { date :: Date,
        open :: Double,
        high :: Double,
        low :: Double,
        close :: Double,
        adjClose :: Double,
        volume :: Integer
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type Parser = M.Parsec Void Text

-- The exception exported by this module
newtype ParserException
  = ParserException String
  deriving (Eq)

instance Show ParserException where
  show (ParserException s) = s

instance Exception ParserException

parseSource :: (MonadThrow m) => Parser a -> FilePath -> Text -> m a
parseSource p f t = case M.parse p f t of
  Left e -> (throwM . ParserException . M.errorBundlePretty) e
  Right d -> pure d

parseCSV :: Parser [TimeSeriesEntry]
parseCSV = catMaybes <$> (parseHeaders *> M.many parseLine)

separator :: Parser ()
separator = void $ M.choice [void $ M.string ",", void M.eol, void M.eof]

field :: Parser a -> Parser a
field = L.lexeme separator

parseHeaders :: Parser ()
parseHeaders =
  void $
    string "Date"
      >> string "Open"
      >> string "High"
      >> string "Low"
      >> string "Close"
      >> string "Adj Close"
      >> string "Volume"

parseLine :: Parser (Maybe TimeSeriesEntry)
parseLine = M.try (Just <$> validLine) M.<|> invalidLine
  where
    validLine :: Parser TimeSeriesEntry
    validLine = TimeSeriesEntry <$> isoDate <*> double <*> double <*> double <*> double <*> double <*> integer
    invalidLine :: Parser (Maybe TimeSeriesEntry)
    invalidLine = (isoDate >> string "null" >> string "null" >> string "null" >> string "null" >> string "null" >> string "null") >> pure Nothing

string :: Text -> Parser Text
string = field . M.string

isoDate :: Parser Date
isoDate = L.lexeme separator M.parseISODate

double :: Parser Double
double = toRealFloat <$> L.lexeme separator L.scientific

integer :: Parser Integer
integer = L.lexeme separator L.decimal

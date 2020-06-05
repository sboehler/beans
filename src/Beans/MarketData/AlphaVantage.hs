module Beans.MarketData.AlphaVantage
  ( getDailySeries,
    getFXSeries,
    APIKey (APIKey),
    FXEntry (..),
    TimeSeriesEntry (..),
    TimeSeriesResult (..),
    TimeSeries (..),
    FXResult (..),
    FX (..),
    Symbol (Symbol),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Network.HTTP.Simple as HTTP

--------------------------------------------------------------------------------
-- High level methods
--------------------------------------------------------------------------------

getDailySeries :: MonadIO m => APIKey -> Symbol -> m TimeSeriesResult
getDailySeries k s = do
  let r = TimeSeriesRequest s Compact JSON k
  HTTP.getResponseBody <$> request r

getFXSeries :: (MonadIO m) => APIKey -> Symbol -> Symbol -> m FXResult
getFXSeries k fromCurrency toCurrency = do
  let r = FXRequest fromCurrency toCurrency Compact JSON k
  HTTP.getResponseBody <$> request r

--------------------------------------------------------------------------------
-- API Implementation
--------------------------------------------------------------------------------

newtype Symbol = Symbol String

instance Show Symbol where
  show (Symbol s) = s

data OutputSize = Compact | Full

instance Show OutputSize where
  show Full = "full"
  show Compact = "compact"

data DataType = JSON | CSV

instance Show DataType where
  show JSON = "json"
  show CSV = "csv"

newtype APIKey = APIKey String

instance Show APIKey where
  show (APIKey k) = k

data Request
  = TimeSeriesRequest
      { symbol :: Symbol,
        outputSize :: OutputSize,
        dataType :: DataType,
        apiKey :: APIKey
      }
  | FXRequest
      { fromCurrency :: Symbol,
        toCurrency :: Symbol,
        outputSize :: OutputSize,
        dataType :: DataType,
        apiKey :: APIKey
      }

baseURL :: String
baseURL = "https://www.alphavantage.co/query"

createURL :: Request -> HTTP.Request
createURL r =
  HTTP.setRequestQueryString (createQuery r)
    . HTTP.parseRequest_
    $ baseURL
  where
    createQuery TimeSeriesRequest {..} =
      fmap
        (fmap (Just . BS.pack))
        [ ("function", "TIME_SERIES_DAILY"),
          ("symbol", show symbol),
          ("apikey", show apiKey),
          ("dataType", show dataType),
          ("outputsize", show outputSize)
        ]
    createQuery FXRequest {..} =
      fmap
        (fmap (Just . BS.pack))
        [ ("function", "FX_DAILY"),
          ("from_symbol", show fromCurrency),
          ("to_symbol", show toCurrency),
          ("apikey", show apiKey),
          ("dataType", show dataType),
          ("outputsize", show outputSize)
        ]

request :: (MonadIO m, FromJSON a) => Request -> m (HTTP.Response a)
request r = HTTP.httpJSON (createURL r)

--------------------------------------------------------------------------------
-- Daily Series
--------------------------------------------------------------------------------

data TimeSeriesResult
  = TimeSeriesResult
      { metaData :: TimeSeriesMetaData,
        timeSeries :: TimeSeries
      }
  deriving (Show)

instance FromJSON TimeSeriesResult where
  parseJSON = withObject "DailySeriesResult" $ \o ->
    TimeSeriesResult <$> o .: "Meta Data" <*> o .: "Time Series (Daily)"

newtype TimeSeriesMetaData
  = TimeSeriesMetaData
      { symbol :: String
      }
  deriving (Show)

instance FromJSON TimeSeriesMetaData where
  parseJSON = withObject "MetaData" $ \o ->
    TimeSeriesMetaData <$> o .: "2. Symbol"

newtype TimeSeries
  = TimeSeries
      { entries :: Map Day TimeSeriesEntry
      }
  deriving (Show)

instance FromJSON TimeSeries where
  parseJSON = withObject "TimeSeries" $ \o -> do
    entries <- for (HM.toList o) $ \(k, v) -> do
      day <- iso8601ParseM $ Text.unpack k
      value <- parseJSON v
      pure (day, value)
    pure . TimeSeries . Map.fromList $ entries

data TimeSeriesEntry
  = TimeSeriesEntry
      { open :: Double,
        high :: Double,
        low :: Double,
        close :: Double,
        volume :: Integer
      }
  deriving (Show)

instance FromJSON TimeSeriesEntry where
  parseJSON = withObject "TimeSeriesEntry" $ \o -> do
    open <- read <$> o .: "1. open"
    high <- read <$> o .: "2. high"
    low <- read <$> o .: "3. low"
    close <- read <$> o .: "4. close"
    volume <- read <$> o .: "5. volume"
    pure $ TimeSeriesEntry {..}

--------------------------------------------------------------------------------
-- FX Series
--------------------------------------------------------------------------------

data FXResult
  = FXResult
      { metaData :: FXMetaData,
        timeSeries :: FX
      }
  deriving (Show)

instance FromJSON FXResult where
  parseJSON = withObject "FXResult" $ \o ->
    FXResult <$> o .: "Meta Data" <*> o .: "Time Series FX (Daily)"

data FXMetaData
  = FXMetaData
      { fromCurrency :: String,
        toCurrency :: String
      }
  deriving (Show)

instance FromJSON FXMetaData where
  parseJSON = withObject "MetaData" $ \o -> do
    fromCurrency <- o .: "2. From Symbol"
    toCurrency <- o .: "3. To Symbol"
    pure FXMetaData {..}

newtype FX
  = FX
      { entries :: Map Day FXEntry
      }
  deriving (Show)

instance FromJSON FX where
  parseJSON = withObject "FXSeries" $ \o -> do
    entries <- for (HM.toList o) $ \(k, v) -> do
      day <- iso8601ParseM $ Text.unpack k
      value <- parseJSON v
      pure (day, value)
    pure . FX . Map.fromList $ entries

data FXEntry
  = FXEntry
      { open :: Double,
        high :: Double,
        low :: Double,
        close :: Double
      }
  deriving (Show)

instance FromJSON FXEntry where
  parseJSON = withObject "TimeSeriesEntry" $ \o -> do
    open <- read <$> o .: "1. open"
    high <- read <$> o .: "2. high"
    low <- read <$> o .: "3. low"
    close <- read <$> o .: "4. close"
    pure $ FXEntry {..}

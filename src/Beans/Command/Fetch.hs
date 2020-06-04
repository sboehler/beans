module Beans.Command.Fetch (run) where

import Beans.Commodity (Commodity)
import Beans.Date (Date (Date))
import qualified Beans.MarketData.AlphaVantage as AV
import Beans.MarketData.AlphaVantage (APIKey (..), FXEntry (..), TimeSeriesEntry (..))
import qualified Beans.MarketData.Yahoo as Y
import Beans.Options (FetchOptions (..))
import qualified Beans.Parser as Parser
import Beans.Price (Price (..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.List as List
import Data.Map.Strict.Extended (Map)
import qualified Data.Map.Strict.Extended as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (pretty, vsep)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import Data.Time.Calendar (Day)
import qualified Dhall
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import qualified System.Environment
import qualified System.FilePath as FilePath
import System.IO (stderr)

newtype Entries = Entries [Entry] deriving (Show, Generic)

instance Dhall.FromDhall Entries

data Entry
  = Entry
      { commodity :: Commodity,
        targetCommodity :: Commodity,
        file :: FilePath,
        config :: Config
      }
  deriving (Show, Generic)

instance Dhall.FromDhall Entry

data Config
  = AVTS
      { symbol :: String
      }
  | AVFX
      { fromCurrency :: String,
        toCurrency :: String
      }
  | YTS {symbol :: String}
  deriving (Show, Generic)

instance Dhall.FromDhall Config

readConfig :: (MonadReader FetchOptions m, MonadIO m) => FilePath -> m Entries
readConfig = liftIO . Dhall.inputFile Dhall.auto

run :: (MonadIO m, MonadReader FetchOptions m, MonadThrow m) => m ()
run = do
  FetchOptions {configFile, commodities} <- ask
  Entries entries <- readConfig configFile
  let baseDir = FilePath.takeDirectory configFile
      filtered = filterEntries commodities entries
  sequence_ $ updateEntry baseDir <$> filtered

filterEntries :: Maybe [Commodity] -> [Entry] -> [Entry]
filterEntries Nothing = id
filterEntries (Just cs) = List.filter (\Entry {commodity} -> commodity `elem` cs)

updateEntry :: (MonadIO m, MonadReader FetchOptions m, MonadThrow m) => FilePath -> Entry -> m ()
updateEntry baseDir entry = liftIO $ do
  let path = FilePath.combine baseDir (file entry)
  exists <- doesFileExist path
  existing <-
    if exists
      then do
        Text.hPutStrLn stderr $ "Parsing file " <> Text.pack path
        parseFile path
      else mempty
  Text.hPutStrLn stderr "Fetching new prices..."
  new <- fetchPrices entry
  let merged = Map.elems $ Map.union new existing
      content = Pretty.renderStrict . Pretty.layoutCompact . vsep . fmap pretty $ merged
  Text.hPutStrLn stderr $ "Writing file " <> Text.pack path
  Text.writeFile path content

-- wait :: MonadIO m => Int -> m ()
-- wait n
--   | n > 0 = liftIO $ do
--     Text.hPutStrLn stderr $ "Waiting " <> (Text.pack . show $ n) <> " seconds until next call"
--     threadDelay 100000
--     wait (n -1)
--   | otherwise = pure ()

parseFile :: (MonadIO m, MonadThrow m) => FilePath -> m (Map Date Price)
parseFile e = Map.fromList . fmap (\p@(Price d _ _ _) -> (d, p)) <$> Parser.parsePrices e

fetchPrices :: (MonadIO m, MonadThrow m) => Entry -> m (Map Date Price)
fetchPrices e@Entry {config} =
  case config of
    AVTS {symbol} -> do
      key <- APIKey <$> liftIO (System.Environment.getEnv "AV_API_KEY")
      AV.TimeSeriesResult {timeSeries = AV.TimeSeries {entries}} <- AV.getDailySeries key (AV.Symbol symbol)
      pure . entriesToPrices e $ entries
    AVFX {toCurrency, fromCurrency} -> do
      key <- APIKey <$> liftIO (System.Environment.getEnv "AV_API_KEY")
      AV.FXResult {timeSeries = AV.FX {entries}} <- AV.getFXSeries key (AV.Symbol fromCurrency) (AV.Symbol toCurrency)
      pure . fxEntriesToPrices e $ entries
    YTS {symbol} -> do
      Y.TimeSeries entries <- Y.getDailySeries (Y.Symbol symbol)
      pure . yEntriesToPrices e $ entries

yEntriesToPrices :: Entry -> [Y.TimeSeriesEntry] -> Map Date Price
yEntriesToPrices Entry {commodity, targetCommodity} = Map.fromList . fmap g
  where
    g (Y.TimeSeriesEntry date _ _ _ _ price _) = (date, Price date commodity price targetCommodity)

entriesToPrices :: Entry -> Map Day TimeSeriesEntry -> Map Date Price
entriesToPrices Entry {commodity, targetCommodity} = Map.fromList . fmap g . Map.toList
  where
    g (d, TimeSeriesEntry {close}) = (date, Price date commodity price targetCommodity)
      where
        date = Date d
        price = close

fxEntriesToPrices :: Entry -> Map Day FXEntry -> Map Date Price
fxEntriesToPrices Entry {commodity, targetCommodity} = Map.fromList . fmap g . Map.toList
  where
    g (d, FXEntry {close}) = (date, Price date commodity price targetCommodity)
      where
        date = Date d
        price = close

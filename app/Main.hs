module Main where

import Beans.Account (AccountFilter (AccountFilter))
import Beans.Commodity (Commodity, CommodityFilter (CommodityFilter))
import Beans.Date (Date, Interval (..))
import Beans.Lib (run)
import qualified Beans.Megaparsec as M
import Beans.Options
  ( BalanceFormat (..),
    BalanceOptions (..),
    Collapse,
    Command (..),
    Diffing (..),
    FetchOptions (..),
    Filter (..),
    ImportOptions (..),
    InferOptions (..),
    TranscodeOptions (..),
  )
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void (Void)
import Options.Applicative
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

toReadM :: M.Parsec Void Text a -> ReadM a
toReadM p = maybeReader $ rightToMaybe . M.parse p "" . Text.pack

journalParser :: Parser FilePath
journalParser =
  strOption $
    short 'j'
      <> long "journal"
      <> metavar "JOURNAL"
      <> help
        "The journal file to parse"

showCommoditiesParser :: Parser Bool
showCommoditiesParser =
  switch $
    long "show-commodities"
      <> short 'c'
      <> help "Show commodities"

percentParser :: Parser (Maybe AccountFilter)
percentParser =
  Just . AccountFilter
    <$> strOption
      (long "percent" <> metavar "REGEX")
    <|> pure Nothing

diffingParser :: Parser Diffing
diffingParser = do
  d <-
    switch
      (long "diff" <> short 'd' <> help "Diff balances")
  pure $ if d then Diffing else NoDiffing

balanceFormatParser :: Parser BalanceFormat
balanceFormatParser =
  g
    <$> strOption
      (long "format" <> short 'f' <> help "The format of th report" <> value "hierarchical")
  where
    g :: String -> BalanceFormat
    g "flat" = Flat
    g _ = Hierarchical

valuationParser :: Parser [Commodity]
valuationParser =
  option
    (toReadM (M.parseCommodity `M.sepBy` M.char ','))
    (long "val" <> metavar "COMMODITY" <> short 'v' <> help "Valuation at market prices")
    <|> pure []

filterParser :: Parser Filter
filterParser = do
  af <-
    AccountFilter
      <$> strOption
        (long "account-filter" <> value "" <> metavar "REGEX")
  cf <-
    CommodityFilter
      <$> strOption
        (long "commodity-filter" <> value "" <> metavar "REGEX")
  pure $ Filter af cf

dateparser :: String -> String -> Parser (Maybe Date)
dateparser optionStr helpStr =
  Just
    <$> option
      (toReadM M.parseISODate)
      (long optionStr <> help helpStr <> metavar "YYYY-MM-DD")
      <|> pure Nothing

collapseParser :: Parser Collapse
collapseParser = many $ option p (short 'p' <> long "collapse" <> metavar "REGEX,DEPTH")
  where
    p = toReadM $ do
      s <- AccountFilter . Text.unpack <$> M.takeWhileP Nothing (/= ',')
      _ <- M.char ','
      d <- L.decimal
      pure (s, d)

fromParser, toParser :: Parser (Maybe Date)
fromParser =
  dateparser "from" "Consider only transactions at or after this date"
toParser =
  dateparser "to" "Consider only transactions before this date"

balanceOptions :: Parser BalanceOptions
balanceOptions =
  BalanceOptions
    <$> journalParser
    <*> valuationParser
    <*> filterParser
    <*> diffingParser
    <*> showCommoditiesParser
    <*> balanceFormatParser
    <*> fromParser
    <*> toParser
    <*> intervalParser
    <*> percentParser
    <*> collapseParser

intervalParser :: Parser (Maybe Interval)
intervalParser = g <$> strOption (metavar "INTERVAL" <> short 'i' <> long "interval") <|> pure Nothing
  where
    g :: String -> Maybe Interval
    g "daily" = Just Daily
    g "weekly" = Just Weekly
    g "monthly" = Just Monthly
    g "quarterly" = Just Quarterly
    g "yearly" = Just Yearly
    g _ = Nothing

commoditiesParser :: Parser (Maybe [Commodity])
commoditiesParser =
  Just
    <$> option
      (toReadM (M.parseCommodity `M.sepBy` M.char ','))
      (long "commodities" <> metavar "COMMODITY" <> short 'c' <> help "The commodity to fetch")
    <|> pure Nothing

configFileParser :: Parser FilePath
configFileParser =
  argument
    str
    ( metavar "CONFIG_FILE"
        <> help
          "The dhall config file to parse"
    )

fetchOptions :: Parser FetchOptions
fetchOptions = FetchOptions <$> commoditiesParser <*> configFileParser

importOptions :: Parser ImportOptions
importOptions =
  ImportOptions
    <$> strOption
      ( metavar "IMPORTER" <> short 'i'
      )
    <*> option
      (toReadM M.parseAccount)
      (metavar "ACCOUNT" <> long "account" <> short 'a')
    <*> argument str (metavar "INPUT_FILE" <> help "The data file to parse")

inferOptions :: Parser InferOptions
inferOptions =
  InferOptions
    <$> strOption
      ( metavar "TRAINING_FILE" <> help "The file containing the training data"
          <> short 't'
          <> long "training-file"
      )
    <*> argument str (metavar "TARGET_FILE")

transcodeOptions :: Parser TranscodeOptions
transcodeOptions =
  TranscodeOptions
    <$> option
      (toReadM M.parseCommodity)
      ( metavar "COMMODITY"
          <> help "The valuation commodity"
          <> long "commodity"
          <> short 'c'
      )
    <*> strOption
      ( metavar "SOURCE_FILE" <> help "The file containing the beans data"
          <> short 's'
          <> long "source-file"
      )
    <*> argument str (metavar "TARGET_FILE")

cmd :: Parser Command
cmd =
  hsubparser $
    command
      "balance"
      (info (Balance <$> balanceOptions) (progDesc "Print a generic balance"))
      <> command
        "fetch"
        (info (Fetch <$> fetchOptions) (progDesc "Fetch latest prices"))
      <> command
        "import"
        (info (Import <$> importOptions) (progDesc "Import transactions"))
      <> command
        "infer"
        (info (Infer <$> inferOptions) (progDesc "Infer accounts"))
      <> command
        "transcode"
        (info (Transcode <$> transcodeOptions) (progDesc "Transcode to beancount"))

parserConfig :: ParserInfo Command
parserConfig =
  info
    (helper <*> cmd)
    (fullDesc <> progDesc "A plain text accounting tool" <> header "beans")

main :: IO ()
main = execParser parserConfig >>= run

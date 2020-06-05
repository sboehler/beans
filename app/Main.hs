module Main where

import qualified Beans.Command.Balance as Balance
import qualified Beans.Command.Fetch as Fetch
import qualified Beans.Command.Import as Import
import qualified Beans.Command.Infer as Infer
import qualified Beans.Command.Transcode as Transcode
import Beans.Commodity (Commodity)
import Beans.Date (Date, Interval (..))
import Beans.Filter (AccountFilter (AccountFilter), CommodityFilter (CommodityFilter), Filter (..))
import Beans.Lib (Command (..), run)
import qualified Beans.Megaparsec as M
import Data.Bool (bool)
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
journalParser = strOption options
  where
    options = short 'j' <> long "journal" <> metavar "JOURNAL" <> help "The journal file to parse"

showCommoditiesParser :: Parser Bool
showCommoditiesParser = switch options
  where
    options = long "show-commodities" <> short 'c' <> help "Show commodities"

percentParser :: Parser (Maybe AccountFilter)
percentParser = optional $ AccountFilter <$> strOption options
  where
    options = long "percent" <> metavar "REGEX"

diffingParser :: Parser Balance.Diffing
diffingParser = bool Balance.Diffing Balance.NoDiffing <$> switch options
  where
    options = long "diff" <> short 'd' <> help "Diff balances"

balanceFormatParser :: Parser Balance.Format
balanceFormatParser = g <$> strOption options
  where
    options = long "format" <> short 'f' <> help "The format of th report" <> value "hierarchical"
    g :: String -> Balance.Format
    g "flat" = Balance.Flat
    g _ = Balance.Hierarchical

valuationParser :: Parser [Commodity]
valuationParser = option parse options <|> pure []
  where
    parse = toReadM (M.parseCommodity `M.sepBy` M.char ',')
    options = long "val" <> metavar "COMMODITY" <> short 'v' <> help "Valuation at market prices"

filterParser :: Parser Filter
filterParser = Filter <$> af <*> cf
  where
    af = AccountFilter <$> strOption (long "account-filter" <> value "" <> metavar "REGEX")
    cf = CommodityFilter <$> strOption (long "commodity-filter" <> value "" <> metavar "REGEX")

dateparser :: String -> String -> Parser (Maybe Date)
dateparser optionStr helpStr = optional $ option parse options
  where
    parse = toReadM M.parseISODate
    options = long optionStr <> help helpStr <> metavar "YYYY-MM-DD"

collapseParser :: Parser Balance.Collapse
collapseParser = many $ option parse options
  where
    options = short 'p' <> long "collapse" <> metavar "REGEX,DEPTH"
    parse = toReadM $ do
      s <- Text.unpack <$> M.takeWhileP Nothing (/= ',')
      _ <- M.char ','
      d <- L.decimal
      pure (AccountFilter s, d)

fromParser, toParser :: Parser (Maybe Date)
fromParser = dateparser "from" "Consider only transactions at or after this date"
toParser = dateparser "to" "Consider only transactions before this date"

balanceOptions :: Parser Balance.Options
balanceOptions =
  Balance.Options
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
intervalParser = parse <$> strOption (metavar "INTERVAL" <> short 'i' <> long "interval")
  where
    parse :: String -> Maybe Interval
    parse "daily" = Just Daily
    parse "weekly" = Just Weekly
    parse "monthly" = Just Monthly
    parse "quarterly" = Just Quarterly
    parse "yearly" = Just Yearly
    parse _ = Nothing

commoditiesParser :: Parser (Maybe [Commodity])
commoditiesParser = optional $ option parse options
  where
    options = long "commodities" <> metavar "COMMODITY" <> short 'c' <> help "The commodity to fetch"
    parse = toReadM $ M.parseCommodity `M.sepBy` M.char ','

configFileParser :: Parser FilePath
configFileParser = argument str options
  where
    options = metavar "CONFIG_FILE" <> help "The dhall config file to parse"

fetchOptions :: Parser Fetch.Options
fetchOptions = Fetch.Options <$> commoditiesParser <*> configFileParser

importOptions :: Parser Import.Config
importOptions =
  Import.Config <$> importer <*> inputFile <*> account
  where
    importer = strOption (metavar "IMPORTER" <> short 'i')
    account = option (toReadM M.parseAccount) (metavar "ACCOUNT" <> long "account" <> short 'a')
    inputFile = argument str (metavar "INPUT_FILE" <> help "The data file to parse")

inferOptions :: Parser Infer.Options
inferOptions =
  Infer.Options <$> trainingFile <*> targetFile
  where
    trainingFile =
      strOption
        ( metavar "TRAINING_FILE" <> help "The file containing the training data"
            <> short 't'
            <> long "training-file"
        )
    targetFile = argument str (metavar "TARGET_FILE")

transcodeOptions :: Parser Transcode.Options
transcodeOptions =
  Transcode.Options
    <$> option
      (toReadM M.parseCommodity)
      ( metavar "COMMODITY"
          <> help "The valuation commodity"
          <> long "commodity"
          <> short 'c'
      )
    <*> strOption
      ( metavar "SOURCE_FILE" <> help "The source file"
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

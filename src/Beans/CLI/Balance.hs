module Beans.CLI.Balance (balanceOptions) where

import           Beans.Options       (BalanceOptions (..), 
                                      ReportType (..))
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Prelude             hiding (filter)

import           Beans.Data.Accounts (CommodityName)
import qualified Beans.Parser        as P
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day)
import           Text.Megaparsec     (parseMaybe)


toReadM :: P.Parser a -> ReadM a
toReadM p = maybeReader $ parseMaybe p . T.pack

convert :: Parser (Maybe CommodityName)
convert =
  optional $ option (toReadM P.commodity) (long "convert" <> short 'c')

lots :: Parser Bool
lots =
  switch (long "lots" <> short 'l')

dateparser :: String -> Parser (Maybe Day)
dateparser l = optional $ option (toReadM P.date) (long l)

parseReportType :: ReadM ReportType
parseReportType =
  eitherReader $ \case
    "hierarchical" -> Right Hierarchical
    "flat" -> Right Flat
    s -> Left $ "Invalid report type: " <> s

reportType :: Parser ReportType
reportType =
  option
    parseReportType
    (value Hierarchical <>
     help "The type of the report, either 'flat' or 'hierarchical' (default)" <>
     long "report-type" <>
     metavar "REPORT_TYPE" <>
     short 'r')


journal :: Parser FilePath
journal =
  option
    str
    (value "journal.bean" <> metavar "JOURNAL" <>
     help "The journal file to parse" <>
     long "journal" <>
     short 'j')

filter :: Parser (Maybe String)
filter =
  optional $
  option
    str
    (metavar "REGEX" <> help "A regular expression to filter the accounts" <>
     long "filter" <>
     short 'f')

strictFilter :: Parser Bool
strictFilter =
  switch
    (long "strict-filter" <> short 's' <>
     help
       "If enabled, strict filtering will filter all postings which don't match. If disabled (default), only transactions are filtered.")


depth :: Parser (Maybe Int)
depth =
  optional $
  option
    auto
    (metavar "DEPTH" <> help "summarize accounts at level DEPTH" <> long "depth" <>
     short 'd')

balanceOptions :: Parser BalanceOptions
balanceOptions =
  BalanceOptions <$> journal <*> convert <*> lots <*> dateparser "from" <*>
  dateparser "to" <*>
  depth <*>
  filter <*>
  strictFilter <*>
  reportType

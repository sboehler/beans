module Beans.CLI.Balance (balanceOptions) where

import           Beans.Options       (BalanceOptions (..), ReportType (..))
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Prelude             hiding (filter)

import           Beans.CLI.Common    (toReadM)
import           Beans.Data.Accounts (Commodity)
import qualified Beans.Parser        as P
import           Data.Time.Calendar  (Day)


convert :: Parser (Maybe Commodity)
convert =
  optional $ option (toReadM P.commodity) (long "convert" <> short 'c')

lots :: Parser Bool
lots =
  switch (long "lots" <> short 'l')

dateparser :: String -> String -> Parser (Maybe Day)
dateparser optionStr helpStr =
  optional $
  option (toReadM P.date) (long optionStr <> help helpStr <> metavar "YYYY-MM-DD")

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
     help "The type of the report" <>
     long "report-type" <>
     metavar "(flat|hierarchical)" <>
     short 'r')


journal :: Parser FilePath
journal =
  argument
    str
    (value "journal.bean" <> metavar "<path>" <>
     help "The journal file to parse")

filter :: Parser (Maybe String)
filter =
  optional $
  option
    str
    (metavar "<regex>" <> help "A regular expression to filter the accounts" <>
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
    (metavar "<int>" <> help "summarize accounts at this level" <> long "depth" <>
     short 'd')

balanceOptions :: Parser BalanceOptions
balanceOptions =
  BalanceOptions <$> journal <*> convert <*> lots <*>
  dateparser "from" "Consider only transactions at or after this date" <*>
  dateparser "to" "Consider only transaction before or at this date"<*>
  depth <*>
  filter <*>
  strictFilter <*>
  reportType

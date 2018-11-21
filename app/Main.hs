module Main where

import           Beans.Model                              ( Account(..)
                                                          , AccountType(Equity)
                                                          , Filter(..)
                                                          , Date(..)
                                                          )
import           Beans.Lib                                ( run )
import           Beans.Options                            ( BalanceOptions(..)
                                                          , Command(..)
                                                          , ImportOptions(..)
                                                          , JournalOptions(..)
                                                          , ReportType(..)
                                                          , Valuation(..)
                                                          )
import qualified Beans.Parser                  as P
import           Data.Bifunctor                           ( first )
import           Data.Semigroup                           ( (<>) )
import qualified Data.Text                     as T
import           Options.Applicative
import           Text.Megaparsec                          ( parse
                                                          , parseErrorPretty
                                                          )


toReadM :: P.Parser a -> ReadM a
toReadM p = eitherReader $ first parseErrorPretty . parse p "" . T.pack

dateparser :: Date -> String -> String -> Parser Date
dateparser v optionStr helpStr = option
  (toReadM P.pDate)
  (value v <> long optionStr <> help helpStr <> metavar "YYYY-MM-DD")

fromParser, toParser :: Parser Date
fromParser =
  dateparser MinDate "from" "Consider only transactions at or after this date"
toParser =
  dateparser MaxDate "to" "Consider only transactions before this date"

filterParser :: Parser Filter
filterParser =
  (Filter <$> strOption
      (long "filter" <> metavar "REGEX" <> short 'f' <> help
        "Filter the postings with the given regex."
      )
    )
    <|> (StrictFilter <$> strOption
          (long "strict-filter" <> metavar "REGEX" <> help
            "Filter the transactions with the given regex."
          )
        )
    <|> pure NoFilter

regexParser :: Parser T.Text
regexParser = T.pack <$> strOption
  (long "filter" <> metavar "REGEX" <> short 'f' <> help
    "Filter the accounts with the given regex."
  )

valuationParser :: Parser Valuation
valuationParser =
  let account = Account Equity ["Valuation"]
  in
    (   AtMarket
    <$> option
          (toReadM P.commodity)
          (long "at-market" <> metavar "COMMODITY" <> short 'm' <> help
            "Valuation at market prices"
          )
    <*> pure account
    )
    <|> (AtCost <$> option
          (toReadM P.commodity)
          (long "at-cost" <> metavar "COMMODITY" <> help
            "Valuation at recorded costs"
          )
        )
    <|> pure NoValuation

journalParser :: Parser FilePath
journalParser = strOption
  (short 'j' <> long "journal" <> metavar "JOURNAL" <> help
    "The journal file to parse"
  )


balanceOptions :: Parser BalanceOptions
balanceOptions =
  BalanceOptions
    <$> journalParser
    <*> valuationParser
    <*> switch (long "lots" <> short 'l' <> help "Show lots")
    <*> fromParser
    <*> toParser
    <*> optional
          (option
            auto
            (  metavar "DEPTH"
            <> help "summarize accounts at this level"
            <> long "depth"
            <> short 'd'
            )
          )
    <*> filterParser
    <*> flag Hierarchical Flat (long "flat" <> help "Show a flat report")


importOptions :: Parser ImportOptions
importOptions =
  ImportOptions
    <$> strOption
          (metavar "IMPORTER" <> short 'i' <> help
            "Currently: only ch.postfinance"
          )
    <*> strOption
          (  metavar "CONFIG_FILE"
          <> help "The configuration to use"
          <> short 'c'
          <> long "config"
          )
    <*> option (toReadM P.account)
               (metavar "ACCOUNT" <> long "account" <> short 'a')
    <*> argument str (metavar "<data file>" <> help "The data file to parse")

journalOptions :: Parser JournalOptions
journalOptions =
  JournalOptions
    <$> journalParser
    <*> valuationParser
    <*> fromParser
    <*> toParser
    <*> regexParser

cmd :: Parser Command
cmd =
  hsubparser
    $  command
         "balance"
         (info (Balance <$> balanceOptions) (progDesc "Print a generic balance"))
    <> command
         "incomeStatement"
         (info (IncomeStatement <$> balanceOptions)
               (progDesc "Print a formatted income statement")
         )
    <> command
         "balanceSheet"
         (info (BalanceSheet <$> balanceOptions)
               (progDesc "Print a formatted balance sheet")
         )
    <> command "import"
               (info (Import <$> importOptions) (progDesc "Import data"))
    <> command
         "journal"
         (info
           (Journal <$> journalOptions)
           (progDesc "Show the changes in one or several accounts over a period"
           )
         )

parserConfig :: ParserInfo Command
parserConfig = info
  (helper <*> cmd)
  (fullDesc <> progDesc "A plain text accounting tool" <> header "beans")

main :: IO ()
main = execParser parserConfig >>= run

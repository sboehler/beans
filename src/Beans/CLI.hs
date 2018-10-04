module Beans.CLI
  ( balanceOptions, importOptions
  ) where

import           Beans.Options       (BalanceOptions (..), Filter (..),
                                      ImportOptions (..), ReportType (..),
                                      Valuation (..))
import qualified Beans.Parser        as P
import           Data.Bifunctor      (first)
import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Data.Time.Calendar  (Day)
import           Options.Applicative
import           Text.Megaparsec     (parse, parseErrorPretty)


toReadM :: P.Parser a -> ReadM a
toReadM p = eitherReader $ first parseErrorPretty . parse p "" . T.pack

dateparser :: String -> String -> Parser (Maybe Day)
dateparser optionStr helpStr = optional $ option
  (toReadM P.date)
  (long optionStr <> help helpStr <> metavar "YYYY-MM-DD")

filterParser :: Parser Filter
filterParser =
  ( Filter <$> strOption
      ( long "filter" <> metavar "REGEX" <> short 'f' <> help
        "Filter the postings with the given regex."
      )
    )
    <|> ( StrictFilter <$> strOption
          ( long "strict-filter" <> metavar "REGEX" <> help
            "Filter the transactions with the given regex."
          )
        )
    <|> pure NoFilter

valuationParser :: Parser Valuation
valuationParser =
  ( AtMarket <$> option
      (toReadM P.commodity)
      ( long "at-market" <> metavar "COMMODITY" <> short 'm' <> help
        "Valuation at market prices"
      )
    )
    <|> ( AtCost <$> option
          (toReadM P.commodity)
          ( long "at-cost" <> metavar "COMMODITY" <> help
            "Valuation at recorded costs"
          )
        )
    <|> pure NoValuation

balanceOptions :: Parser BalanceOptions
balanceOptions =
  BalanceOptions
    <$> strOption
          ( short 'j' <> long "journal" <> metavar "JOURNAL" <> help
            "The journal file to parse"
          )
    <*> valuationParser
    <*> switch (long "lots" <> short 'l' <> help "Show lots")
    <*> dateparser "from" "Consider only transactions at or after this date"
    <*> dateparser "to"   "Consider only transaction before or at this date"
    <*> optional
          ( option
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
          ( metavar "IMPORTER" <> short 'i' <> help
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

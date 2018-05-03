module Haricot.CLI where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import qualified Data.Text.Lazy      as T
import           Data.Time.Calendar  (Day)
import           Haricot.AST         (CommodityName)
import qualified Haricot.Parser      as P
import           Text.Megaparsec     (parseMaybe)

data Options = Options
  { optJournal       :: FilePath
  , optMarkToMmarket :: Maybe CommodityName
  , optFrom          :: Maybe Day
  , optTo            :: Maybe Day
  , optCommand       :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)

toReadM :: P.Parser a -> ReadM a
toReadM p = maybeReader $ parseMaybe p . T.pack

markToMarket :: Parser (Maybe CommodityName)
markToMarket =
  optional $ option (toReadM P.commodity) (long "mark-to-market" <> short 'm')

dateparser :: String -> Parser (Maybe Day)
dateparser l =
  optional $ option (toReadM P.date) (long l)

journal :: Parser FilePath
journal =
  option
    str
    (value "journal.bean" <> metavar "JOURNAL" <>
     help "The journal file to parse" <>
     long "journal" <>
     short 'f')

cmd :: Parser Command
cmd =
  subparser $
  command "balance" (info (pure Balance) (progDesc "Print a balance sheet"))

config :: Parser Options
config =
  Options <$> journal <*> markToMarket <*> dateparser "from" <*> dateparser "to" <*> cmd

parserConfig :: ParserInfo Options
parserConfig =
  info
    (helper <*> config)
    (fullDesc <> progDesc "Evaluate a haricot journal" <>
     header "haricot - a plain text accounting tool")

main :: IO ()
main = execParser parserConfig >>= run

run :: Options -> IO()
run options@Options {..}= case optCommand of
  Balance -> runBalance options

runBalance :: Options -> IO()
runBalance = print
  

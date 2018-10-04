module Main where

import           Beans.CLI           (balanceOptions, importOptions)
import           Beans.Lib           (runBeans)
import           Beans.Options       (Command (..))
import           Data.Semigroup      ((<>))
import           Options.Applicative


cmd :: Parser Command
cmd =
  hsubparser $
  command
    "balance"
    (info (Balance <$> balanceOptions) (progDesc "Print a balance sheet")) <>
  command "import" (info (Import <$> importOptions) (progDesc "Import data"))

parserConfig :: ParserInfo Command
parserConfig =
  info
    (helper <*> cmd)
    (fullDesc <> progDesc "A plain text accounting tool" <>
     header "beans")

main :: IO ()
main = execParser parserConfig >>= runBeans

module Main
  ( main,
  )
where

import App (startApp)
import Options.Applicative

fileParser :: Parser FilePath
fileParser = argument str (metavar "FILE" <> action "file")

main :: IO ()
main = do
  filepath <- execParser $ (info fileParser) (progDesc "A database file")
  startApp filepath

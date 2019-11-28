module Main
  ( main,
  )
where

import App (startApp)

main :: IO ()
main = startApp "test.db"

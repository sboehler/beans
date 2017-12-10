module Parser where

import Data.Decimal (Decimal)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Model as M
import Text.Parsec
import Text.Parsec.Number

type Parser a = Parsec Text () a

date :: Parser Day
date = do
  year <- read <$> count 4 digit
  _ <- char '-'
  month <- read <$> count 2 digit
  _ <- char '-'
  day <- read <$> count 2 digit
  return $ fromGregorian year month day

flag :: Parser Char
flag = oneOf "!*"

doublequote :: Parser Char
doublequote = char '\"'

description :: Parser Text
description =
  pack <$> do
    _ <- doublequote
    text <- many (noneOf "\"")
    _ <- doublequote
    return text

accountSegment :: Parser Text
accountSegment = do
  x <- letter
  xs <- many alphaNum
  return $ pack (x : xs)

colon :: Parser Char
colon = char ':'

account :: Parser M.AccountName
account = M.AccountName <$> sepBy accountSegment colon

amount :: Parser Decimal
amount = sign <*> fractional2 True

commodity :: Parser M.Commodity
commodity = M.Commodity . pack <$> many1 alphaNum

posting :: Parser M.Posting
posting = M.Posting <$> account <*> amount <*> commodity

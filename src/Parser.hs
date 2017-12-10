module Parser where

import Data.Decimal (Decimal)
import Data.Text (Text, cons, pack)
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
description = pack <$> (doublequote >> many (noneOf "\"") <* doublequote)

accountSegment :: Parser Text
accountSegment = cons <$> letter <*> (pack <$> many alphaNum)

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

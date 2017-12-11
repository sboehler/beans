module Parser where

import Data.Decimal (Decimal)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Model as M
import Text.Parsec hiding (spaces, token)
import Text.Parsec.Number

type Parser a = Parsec Text () a

intN :: (Num a, Read a) => Int -> Parser a
intN n = read <$> count n digit

dash :: Parser Char
dash = char '-'

date :: Parser Day
date = fromGregorian <$> (intN 4 <* dash) <*> (intN 2 <* dash) <*> intN 2

flag :: Parser Char
flag = oneOf "!*"

atLeastOneSpace :: Parser String
atLeastOneSpace = many1 $ char ' '

eol :: Parser Char
eol = many (char ' ') >> newline

token :: Parser a -> Parser a
token p = do
  _ <- many $ char ' '
  r <- p
  _ <- many $ char ' '
  return r

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
posting = M.Posting <$> token account <*> token amount <*> token commodity

wildcardPosting :: Parser M.Posting
wildcardPosting = M.WildcardPosting <$> token account

transaction :: Parser M.Transaction
transaction =
  M.T <$> token date <*> token flag <*> token description <*>
  many1 (try (newline >> char ' ' >> (try posting <|> wildcardPosting)))

emptyLine :: Parser Char
emptyLine = token eol

parse' :: FilePath -> Text -> Either ParseError [M.Transaction]
parse' = parse (many emptyLine >> sepEndBy transaction (many (token newline)))

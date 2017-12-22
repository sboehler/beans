module Parser
  ( parse'
  , AccountName(..)
  , CommodityName(..)
  , Directive(..)
  , DatedDirective(..)
  , ConfigDirective(..)
  , Posting(..)
  , PostingPrice(..)
  ) where

import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Parser.AST
import Text.Parsec
       (ParseError, Parsec, (<|>), alphaNum, anyChar, between, char,
        count, digit, eof, letter, many, many1, manyTill, newline, noneOf,
        oneOf, optionMaybe, parse, sepBy, string, try)
import Text.Parsec.Number (fractional2, sign)

-- The parser monad without state
type Parser a = Parsec Text () a

-- wraps a parser p, consuming all spaces after p
token :: Parser a -> Parser a
token p = do
  result <- p
  _ <- many $ char ' '
  return result

-- primitives
date :: Parser Day
date = token (fromGregorian <$> year <*> month <*> day)
  where
    getInt n = read <$> count n digit
    year = getInt 4
    month = dash *> getInt 2
    day = dash *> getInt 2
    dash = char '-'

eol :: Parser ()
eol = void $ token newline

comment :: Parser ()
comment = void (oneOf ";#" >> manyTill anyChar (try eol))

text :: Parser Char -> Parser Text
text p = pack <$> many p

text1 :: Parser Char -> Parser Text
text1 p = token $ pack <$> many1 p

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

symbol :: String -> Parser Text
symbol i = token $ pack <$> string i

quotedString :: Parser Text
quotedString = token $ quoted textWithoutQuotes
  where
    quoted = surroundedBy $ char quote
    textWithoutQuotes = text $ noneOf [quote]
    quote = '\"'

braces :: Parser a -> Parser a
braces = between brOpen brClose
  where
    brOpen = symbol "{"
    brClose = symbol "}"

decimal :: Parser Decimal
decimal = token $ sign <*> fractional2 True

-- domain objects
accountName :: Parser AccountName
accountName = AccountName <$> name
  where
    name = token $ sepBy segment colon
    segment = cons <$> letter <*> text alphaNum
    colon = char ':'

commodityName :: Parser CommodityName
commodityName = CommodityName <$> text1 alphaNum

postingAmount :: Parser PostingAmount
postingAmount =
  PostingAmount <$> decimal <*> commodityName <*> optionMaybe cost <*>
  optionMaybe (unitPrice <|> totalPrice)
  where
    cost = braces (Cost <$> decimal <*> commodityName <*> optionMaybe label)
    label = symbol "," >> date
    unitPrice = try $ symbol "@" >> UnitPrice <$> decimal <*> commodityName
    totalPrice = try $ symbol "@@" >> TotalPrice <$> decimal <*> commodityName

posting :: Parser Posting
posting = symbol " " >> Posting <$> accountName <*> optionMaybe postingAmount

transaction :: Parser DatedDirective
transaction = Transaction <$> flag <*> quotedString <*> tags <*> postings
  where
    postings = many1 (try (newline >> posting))
    tags = many $ Tag <$> (cons <$> hash <*> text alphaNum)
    hash = char '#'
    flag = token (complete <|> incomplete)
      where
        incomplete = char '!' >> pure Incomplete
        complete = char '*' >> pure Complete

open :: Parser DatedDirective
open =
  symbol "open" >>
  (AccountOpen <$> accountName <*> sepBy commodityName (symbol ","))

close :: Parser DatedDirective
close = symbol "close" >> (AccountClose <$> accountName)

balance :: Parser DatedDirective
balance =
  symbol "balance" >> (Balance <$> accountName <*> decimal <*> commodityName)

price :: Parser DatedDirective
price =
  symbol "price" >> (Price <$> commodityName <*> decimal <*> commodityName)

include :: Parser ConfigDirective
include = symbol "include" >> (Include <$> filePath)
  where
    filePath = unpack <$> quotedString

option :: Parser ConfigDirective
option = symbol "option" >> (Option <$> quotedString <*> quotedString)

directive :: Parser Directive
directive = datedDirective <|> configDirective
  where
    datedDirective =
      Dated <$> date <*> (transaction <|> open <|> close <|> balance <|> price)
    configDirective = Config <$> (include <|> option)

block :: Parser a -> Parser a
block = surroundedBy skipLines
  where
    skipLines = many (comment <|> eol)

directives :: Parser [Directive]
directives = do
  ds <- many (block directive)
  eof
  return ds

parse' :: FilePath -> Text -> Either ParseError [Directive]
parse' = parse directives

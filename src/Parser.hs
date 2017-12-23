module Parser
  ( parse'
  , AccountName(..)
  , CommodityName(..)
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
        oneOf, optionMaybe, parse, sepBy, sepBy1, string, try)
import Text.Parsec.Number (fractional2, sign)

-- The parser monad without state
type Parser = Parsec Text ()

-- wraps a parser p, consuming all spaces after p
token :: Parser a -> Parser a
token p = p <* many (char ' ')

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
    name = token $ segment `sepBy` char ':'
    segment = cons <$> letter <*> text alphaNum

commodityName :: Parser CommodityName
commodityName = CommodityName <$> text1 alphaNum

amount :: Parser Amount
amount = Amount <$> decimal <*> commodityName

postingPrice :: Parser PostingPrice
postingPrice = unitPrice <|> totalPrice
  where
    unitPrice = try $ symbol "@" >> UnitPrice <$> amount
    totalPrice = try $ symbol "@@" >> TotalPrice <$> amount

postingCost :: Parser [PostingCost]
postingCost = cost <|> pure []
  where
    cost = braces $ costElem `sepBy1` symbol ","
    costElem =
      PostingCostAmount <$> try amount <|> PostingCostDate <$> try date <|>
      PostingCostLabel <$> quotedString

postingAmount :: Parser PostingAmount
postingAmount =
  PostingAmount <$> amount <*> postingCost <*> optionMaybe postingPrice

posting :: Parser Posting
posting = symbol " " >> Posting <$> accountName <*> optionMaybe postingAmount

transaction :: Parser DatedDirective
transaction = Transaction <$> flag <*> quotedString <*> tags <*> postings
  where
    postings = many1 (try (newline >> posting))
    tags = many $ Tag <$> (cons <$> char '#' <*> text alphaNum)
    flag = token (complete <|> incomplete)
      where
        incomplete = char '!' >> pure Incomplete
        complete = char '*' >> pure Complete

open :: Parser DatedDirective
open =
  symbol "open" >>
  (AccountOpen <$> accountName <*> commodityName `sepBy` symbol ",")

close :: Parser DatedDirective
close = symbol "close" >> AccountClose <$> accountName

balance :: Parser DatedDirective
balance = symbol "balance" >> Balance <$> accountName <*> amount

price :: Parser DatedDirective
price = symbol "price" >> Price <$> commodityName <*> amount

include :: Parser ConfigDirective
include = symbol "include" >> Include <$> filePath
  where
    filePath = unpack <$> quotedString

option :: Parser ConfigDirective
option = symbol "option" >> (Option <$> quotedString <*> quotedString)

datedDirective :: Parser DatedDirective
datedDirective = transaction <|> open <|> close <|> balance <|> price

configDirective :: Parser ConfigDirective
configDirective = include <|> option

directive :: Parser Directive
directive = dated <|> config
  where
    dated = Dated <$> date <*> datedDirective
    config = Config <$> configDirective

block :: Parser a -> Parser a
block = surroundedBy skipLines
  where
    skipLines = many (comment <|> eol)

directives :: Parser [Directive]
directives = many (block directive) <* eof

parse' :: FilePath -> Text -> Either ParseError [Directive]
parse' = parse directives

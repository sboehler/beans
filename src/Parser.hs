module Parser
  ( parse'
  , AccountName(..)
  , CommodityName(..)
  , Directive(..)
  , Posting(..)
  , PostingPrice(..)
  ) where

import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Text.Parsec
       (ParseError, Parsec, (<|>), alphaNum, anyChar, between, char,
        count, digit, eof, letter, many, many1, manyTill, newline, noneOf,
        oneOf, optionMaybe, parse, sepBy, string, try)
import Text.Parsec.Number (fractional2, sign)

-- The parser monad without state
type Parser a = Parsec Text () a

-- Type to wrap the AST of a file
data Directive
  = Transaction { _date :: Day
                , _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | AccountOpen { _date :: Day
                , _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _date :: Day
                 , _accountName :: AccountName }
  | Balance { _date :: Day
            , _accountName :: AccountName
            , _amount :: Decimal
            , _commodity :: CommodityName }
  | Price { _date :: Day
          , _commodity :: CommodityName
          , _price :: Decimal
          , _priceCommodity :: CommodityName }
  | Include FilePath
  | Option Text
           Text
  deriving (Show)

data Flag
  = Complete
  | Incomplete
  deriving (Show)

newtype Tag =
  Tag Text
  deriving (Show)

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe PostingAmount
  } deriving (Show)

newtype AccountName =
  AccountName [Text]
  deriving (Show)

data PostingAmount = PostingAmount
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _cost :: Maybe Cost
  , _price :: Maybe PostingPrice
  } deriving (Show)

newtype CommodityName =
  CommodityName Text
  deriving (Show)

data Cost = Cost
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _label :: Maybe Day
  } deriving (Show)

data PostingPrice
  = UnitPrice { _amount :: Decimal
              , _commodity :: CommodityName }
  | TotalPrice { _amount :: Decimal
               , _commodity :: CommodityName }
  deriving (Show)

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

transaction :: Day -> Parser Directive
transaction d = Transaction d <$> flag <*> quotedString <*> tags <*> postings
  where
    postings = many1 (try (newline >> posting))
    tags = many $ Tag <$> (cons <$> hash <*> text alphaNum)
    hash = char '#'
    flag = token (complete <|> incomplete)
      where
        incomplete = char '!' >> pure Incomplete
        complete = char '*' >> pure Complete

open :: Day -> Parser Directive
open d =
  symbol "open" >>
  (AccountOpen d <$> accountName <*> sepBy commodityName (symbol ","))

close :: Day -> Parser Directive
close d = symbol "close" >> (AccountClose d <$> accountName)

balance :: Day -> Parser Directive
balance d =
  symbol "balance" >> (Balance d <$> accountName <*> decimal <*> commodityName)

price :: Day -> Parser Directive
price d =
  symbol "price" >> (Price d <$> commodityName <*> decimal <*> commodityName)

datedDirective :: Parser Directive
datedDirective = do
  d <- date
  transaction d <|> open d <|> close d <|> balance d <|> price d

include :: Parser Directive
include = symbol "include" >> (Include <$> filePath)
  where
    filePath = unpack <$> quotedString

option :: Parser Directive
option = symbol "option" >> (Option <$> quotedString <*> quotedString)

directive :: Parser Directive
directive = datedDirective <|> include <|> option

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

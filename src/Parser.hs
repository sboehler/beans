module Parser
  ( parse'
  , Directive(..)
  , CommodityName(..)
  , Amount(..)
  , Posting(..)
  , AccountName(..)
  ) where

import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Debug.Trace
import Text.Parsec
       (ParseError, Parsec, (<|>), alphaNum, anyChar, between, char,
        count, digit, eof, letter, many, many1, manyTill, newline, noneOf,
        oneOf, optionMaybe, parse, sepBy, string, try)
import Text.Parsec.Number (fractional2, sign)

-- The parser monad without state
type Parser a = Parsec Text () a

-- Type to wrap the AST of a file
newtype CommodityName =
  CommodityName Text
  deriving (Show)

data Amount = Amount
  { _amount :: Decimal
  , _commodity :: CommodityName
  } deriving (Show)

data Cost = Cost
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _label :: Maybe Day
  } deriving (Show)

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe Amount
  , _cost :: Maybe Cost
  , _price :: Maybe Amount
  , _totalPrice :: Maybe Amount
  } deriving (Show)

newtype AccountName =
  AccountName [Text]
  deriving (Show)

newtype Tag =
  Tag Text
  deriving (Show)

data Directive
  = Transaction { _date :: Day
                , _flag :: Char
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

token :: Parser a -> Parser a
token p = do
  result <- p
  _ <- spaces
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

flag :: Parser Char
flag = token (char '!' <|> char '*')

spaces :: Parser ()
spaces = void (many $ char ' ')

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

ident :: String -> Parser Text
ident i = token $ pack <$> string i

quotedString :: Parser Text
quotedString = token $ surroundedBy quote textWithoutQuotes
  where
    quote = char '\"'
    textWithoutQuotes = text $ noneOf "\""

braces :: Parser a -> Parser a
braces = between brOpen brClose
  where
    brOpen = ident "{"
    brClose = ident "}"

-- domain objects
accountName :: Parser AccountName
accountName = AccountName <$> name
  where
    name = token $ sepBy segment colon
    segment = cons <$> letter <*> text alphaNum
    colon = char ':'

decimal :: Parser Decimal
decimal = token $ sign <*> fractional2 True

commodityName :: Parser CommodityName
commodityName = CommodityName <$> text1 alphaNum

posting :: Parser Posting
posting =
  Posting <$> accountName <*> amount <*> cost <*> unitPrice <*> totalPrice
  where
    amount = optionMaybe $ Amount <$> decimal <*> commodityName
    cost = optionMaybe $ braces (Cost <$> decimal <*> commodityName <*> label)
    unitPrice =
      optionMaybe $ try $ ident "@" >> (Amount <$> decimal <*> commodityName)
    totalPrice =
      optionMaybe $ try $ ident "@@" >> Amount <$> decimal <*> commodityName
    label = optionMaybe (ident "," >> date)

transaction :: Day -> Parser Directive
transaction d = Transaction d <$> flag <*> quotedString <*> tags <*> postings
  where
    postings = many1 (try (newline >> space >> posting))
    tags = many $ Tag <$> (cons <$> hash <*> text alphaNum)
    hash = char '#'
    space = ident " "

open :: Day -> Parser Directive
open d =
  ident "open" >>
  (AccountOpen d <$> accountName <*> sepBy commodityName (ident ","))

close :: Day -> Parser Directive
close d = ident "close" >> (AccountClose d <$> accountName)

balance :: Day -> Parser Directive
balance d =
  ident "balance" >> (Balance d <$> accountName <*> decimal <*> commodityName)

price :: Day -> Parser Directive
price d =
  ident "price" >> (Price d <$> commodityName <*> decimal <*> commodityName)

datedDirective :: Parser Directive
datedDirective = do
  d <- date
  transaction d <|> open d <|> close d <|> balance d <|> price d

include :: Parser Directive
include = ident "include" >> (Include <$> filePath)
  where
    filePath = unpack <$> quotedString

option :: Parser Directive
option = ident "option" >> (Option <$> quotedString <*> quotedString)

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

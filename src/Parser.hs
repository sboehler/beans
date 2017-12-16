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
import Text.Parsec
       (ParseError, Parsec, (<?>), (<|>), alphaNum, anyChar, between,
        char, count, digit, eof, letter, many, many1, manyTill, newline,
        noneOf, oneOf, optionMaybe, parse, sepBy, string, try)
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

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe Amount
  } deriving (Show)

newtype AccountName =
  AccountName [Text]
  deriving (Show)

data Directive
  = Transaction { _date :: Day
                , _flag :: Char
                , _description :: Text
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

-- primitives
dash :: Parser Char
dash = char '-'

comma :: Parser Char
comma = char ','

space :: Parser Char
space = char ' '

colon :: Parser Char
colon = char ':'

doublequote :: Parser Char
doublequote = char '\"'

date :: Parser Day
date = fromGregorian <$> year <*> month <*> day
  where
    getInt n = read <$> count n digit
    year = getInt 4
    month = dash *> getInt 2
    day = dash *> getInt 2

flag :: Parser Char
flag = oneOf "!*"

spaces :: Parser ()
spaces = void (many space)

emptyline :: Parser ()
emptyline = void (spaces >> newline)

comment :: Parser ()
comment = void (oneOf ";#" >> manyTill anyChar (try newline))

text :: Parser Char -> Parser Text
text p = pack <$> many p

text1 :: Parser Char -> Parser Text
text1 p = pack <$> many1 p

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

token :: Parser a -> Parser a
token p = do
  result <- p
  _ <- spaces
  return result

ident :: String -> Parser Text
ident i = pack <$> token (string i)

quotedString :: Parser Text
quotedString = surroundedBy doublequote (text $ noneOf "\"")

-- domain objects
accountName :: Parser AccountName
accountName = AccountName <$> sepBy segment colon
  where
    segment = cons <$> letter <*> text alphaNum

decimal :: Parser Decimal
decimal = sign <*> fractional2 True

commodityName :: Parser CommodityName
commodityName = CommodityName <$> text1 alphaNum

posting :: Parser Posting
posting = do
  name <- token accountName
  amount <- optionMaybe (Amount <$> token decimal <*> token commodityName)
  return $ Posting name amount

transaction :: Day -> Parser Directive
transaction d = Transaction d <$> token flag <*> token quotedString <*> postings
  where
    postings = many1 (try (newline >> token space >> posting))

accountOpen :: Day -> Parser Directive
accountOpen d =
  AccountOpen d <$> try (ident "open" *> token accountName) <*>
  sepBy (token commodityName) comma

accountClose :: Day -> Parser Directive
accountClose d = AccountClose d <$> try (ident "close" *> token accountName)

balance :: Day -> Parser Directive
balance d =
  Balance d <$> try (ident "balance" *> token accountName) <*> token decimal <*>
  token commodityName

price :: Day -> Parser Directive
price d =
  Price d <$> (ident "price" *> token commodityName) <*> token decimal <*>
  token commodityName

datedDirective :: Parser Directive
datedDirective = do
  d <- token date
  transaction d <|> accountOpen d <|> accountClose d <|> balance d <|>
    price d <?> "dated directive"

include :: Parser Directive
include = Include <$> (ident "include" *> (unpack <$> token quotedString))

option :: Parser Directive
option =
  Option <$> (ident "option" *> token quotedString) <*> token quotedString

directive :: Parser Directive
directive = datedDirective <|> include <|> option <?> "directive"

block :: Parser a -> Parser a
block p = do
  _ <- many (comment <|> emptyline)
  res <- p
  _ <- many (comment <|> emptyline)
  return res

directives :: Parser [Directive]
directives = do
  ds <- many (block directive)
  eof
  return ds

parse' :: FilePath -> Text -> Either ParseError [Directive]
parse' = parse directives

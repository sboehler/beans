module Parser
  ( parse'
  , AccountName(..)
  , CommodityName(..)
  , Directive(..)
  , ConfigDirective(..)
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
data DatedDirective
  = Transaction { _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | AccountOpen { _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _accountName :: AccountName }
  | Balance { _accountName :: AccountName
            , _amount :: Decimal
            , _commodity :: CommodityName }
  | Price { _commodity :: CommodityName
          , _price :: Decimal
          , _priceCommodity :: CommodityName }
  deriving (Show, Eq)

data ConfigDirective
  = Include FilePath
  | Option Text
           Text
  deriving (Show, Eq)

-- Type to wrap the AST of a file
data Directive
  = Config ConfigDirective
  | Dated { _date :: Day
          , _directive :: DatedDirective }
  deriving (Show, Eq)

data Flag
  = Complete
  | Incomplete
  deriving (Show, Eq)

newtype Tag =
  Tag Text
  deriving (Show, Eq)

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe PostingAmount
  } deriving (Show, Eq)

newtype AccountName =
  AccountName [Text]
  deriving (Show, Eq)

data PostingAmount = PostingAmount
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _cost :: Maybe Cost
  , _price :: Maybe PostingPrice
  } deriving (Show, Eq)

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq)

data Cost = Cost
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _label :: Maybe Day
  } deriving (Show, Eq)

data PostingPrice
  = UnitPrice { _amount :: Decimal
              , _commodity :: CommodityName }
  | TotalPrice { _amount :: Decimal
               , _commodity :: CommodityName }
  deriving (Show, Eq)

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

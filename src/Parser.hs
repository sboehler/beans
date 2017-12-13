module Parser where

import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Text (Text, cons, pack)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Model as M
import Text.Parsec
       (ParseError, Parsec, (<|>), alphaNum, anyChar, between, char,
        choice, count, digit, letter, many, many1, manyTill, newline,
        noneOf, oneOf, parse, sepBy, sepEndBy, string, try)
import Text.Parsec.Number (fractional2, sign)

type Parser a = Parsec Text () a

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

spaces :: Parser String
spaces = many space

eol :: Parser ()
eol = void $ spaces >> newline

comment :: Parser ()
comment = void $ noneOf ['0' .. '9'] >> manyTill anyChar (try newline)

newlines :: Parser ()
newlines = void $ many1 (try eol <|> comment)

text :: Parser Char -> Parser Text
text p = pack <$> many p

text1 :: Parser Char -> Parser Text
text1 p = pack <$> many1 p

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

token :: Parser a -> Parser a
token = surroundedBy spaces

ident :: String -> Parser Text
ident i = pack <$> token (string i)

quotedString :: Parser Text
quotedString = surroundedBy doublequote (text $ noneOf "\"")

accountName :: Parser M.AccountName
accountName = M.AccountName <$> sepBy segment colon
  where
    segment = cons <$> letter <*> text alphaNum

amount :: Parser Decimal
amount = sign <*> fractional2 True

commodity :: Parser M.Commodity
commodity = M.Commodity <$> text1 alphaNum

posting :: Parser M.Posting
posting =
  choice
    [ try $ M.Posting <$> token accountName <*> token amount <*> token commodity
    , try $ M.WildcardPosting <$> token accountName
    ]

transaction :: Parser M.Transaction
transaction = M.T <$> date <*> token flag <*> token quotedString <*> postings
  where
    postings = many1 (try (eol >> space >> posting))

accountOpen :: Parser M.AccountOpen
accountOpen =
  M.AccountOpen <$> date <*> (ident "open" *> token accountName) <*>
  sepBy (token commodity) comma

accountClose :: Parser M.AccountClose
accountClose = M.AccountClose <$> date <*> (ident "close" *> token accountName)

balance :: Parser M.Balance
balance =
  M.Balance <$> date <*> (ident "balance" *> token accountName) <*> token amount <*>
  token commodity

price :: Parser M.Price
price =
  M.Price <$> date <*> (ident "price" *> token commodity) <*> token amount <*>
  token commodity

directive :: Parser M.Directive
directive =
  M.DTransaction <$> try transaction <|> M.DAccountOpen <$> try accountOpen <|>
  M.DAccountClose <$> try accountClose <|>
  M.DBalance <$> try balance <|>
  M.DPrice <$> price

parse' :: FilePath -> Text -> Either ParseError [M.Directive]
parse' = parse (newlines >> sepEndBy directive newlines)

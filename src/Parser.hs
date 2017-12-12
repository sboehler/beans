module Parser where

import Data.Decimal (Decimal)
import Data.Text (Text, cons, pack)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Model as M
import Text.Parsec
       (ParseError, Parsec, alphaNum, between, char, choice, count, digit,
        letter, many, many1, newline, noneOf, oneOf, parse, sepBy,
        sepEndBy, try)
import Text.Parsec.Number (fractional2, sign)

type Parser a = Parsec Text () a

dash :: Parser Char
dash = char '-'

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

eol :: Parser Char
eol = spaces >> newline

newlines :: Parser String
newlines = many eol

text :: Parser Char -> Parser Text
text p = pack <$> many p

text1 :: Parser Char -> Parser Text
text1 p = pack <$> many1 p

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

token :: Parser a -> Parser a
token = surroundedBy spaces

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

parse' :: FilePath -> Text -> Either ParseError [M.Transaction]
parse' = parse (newlines >> sepEndBy transaction newlines)

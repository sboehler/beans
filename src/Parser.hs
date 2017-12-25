module Parser
  ( parse'
  , AccountName(..)
  , CommodityName(..)
  , ConfigDirective(..)
  , DatedDirective(..)
  , Flag(..)
  , Posting(..)
  , PostingCost(..)
  , PostingPrice(..)
  , Tag(..)
  ) where

import Control.Monad (void)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Text.Parsec
       (ParseError, Parsec, (<|>), alphaNum, anyChar, between, char,
        count, digit, eof, letter, many, many1, manyTill, newline, noneOf,
        oneOf, option, optionMaybe, parse, sepBy, sepBy1, string, try)
import Text.Parsec.Number (fractional2, sign)

import Parser.AST

type Parser = Parsec Text ()

space :: Parser Char
space = char ' '

dash :: Parser Char
dash = char '-'

colon :: Parser Char
colon = char ':'

hash :: Parser Char
hash = char '#'

doubleQuote :: Parser Char
doubleQuote = char '\"'

token :: Parser a -> Parser a
token p = p <* many space

readInt :: (Read a) => Int -> Parser a
readInt n = read <$> count n digit

date :: Parser Day
date =
  token $
  fromGregorian <$> readInt 4 <* dash <*> readInt 2 <* dash <*> readInt 2

text :: Parser Char -> Parser Text
text p = pack <$> many p

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy p s = between s s p

symbol :: String -> Parser Text
symbol i = token $ pack <$> string i

quotedString :: Parser Text
quotedString = token $ text (noneOf "\"") `surroundedBy` doubleQuote

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

decimal :: Parser Decimal
decimal = token $ sign <*> fractional2 True

accountNameSegment :: Parser Text
accountNameSegment = cons <$> letter <*> text alphaNum

accountName :: Parser AccountName
accountName = token $ AccountName <$> accountNameSegment `sepBy` colon

commodityName :: Parser CommodityName
commodityName = token $ CommodityName . pack <$> many alphaNum

postingPriceUnit :: Parser PostingPrice
postingPriceUnit = symbol "@" >> UnitPrice <$> decimal <*> commodityName

postingPriceTotal :: Parser PostingPrice
postingPriceTotal = symbol "@@" >> TotalPrice <$> decimal <*> commodityName

postingPrice :: Parser PostingPrice
postingPrice = try postingPriceUnit <|> try postingPriceTotal

postingCostDate :: Parser PostingCost
postingCostDate = PostingCostDate <$> date

postingCostAmount :: Parser PostingCost
postingCostAmount = PostingCostAmount <$> decimal <*> commodityName

postingCostLabel :: Parser PostingCost
postingCostLabel = PostingCostLabel <$> quotedString

postingCostElement :: Parser PostingCost
postingCostElement =
  try postingCostLabel <|> try postingCostDate <|> try postingCostAmount

postingCost :: Parser [PostingCost]
postingCost = braces (postingCostElement `sepBy1` symbol ",")

wildcardPosting :: Parser Posting
wildcardPosting = WildcardPosting <$> accountName

completePosting :: Parser Posting
completePosting =
  CompletePosting <$> accountName <*> decimal <*> commodityName <*>
  option [] postingCost <*>
  optionMaybe postingPrice

posting :: Parser Posting
posting = newline >> symbol " " >> (try completePosting <|> try wildcardPosting)

flagIncomplete :: Parser Flag
flagIncomplete = Incomplete <$ symbol "!"

flagComplete :: Parser Flag
flagComplete = Complete <$ symbol "*"

flag :: Parser Flag
flag = flagComplete <|> flagIncomplete

tag :: Parser Tag
tag = Tag <$> (cons <$> hash <*> text alphaNum)

transactionDirective :: Parser DatedDirective
transactionDirective =
  Transaction <$> flag <*> quotedString <*> many tag <*> many1 (try posting)

openDirective :: Parser DatedDirective
openDirective =
  symbol "open" >>
  AccountOpen <$> accountName <*> commodityName `sepBy` symbol ","

closeDirective :: Parser DatedDirective
closeDirective = symbol "close" >> AccountClose <$> accountName

balanceDirective :: Parser DatedDirective
balanceDirective =
  symbol "balance" >> Balance <$> accountName <*> decimal <*> commodityName

priceDirective :: Parser DatedDirective
priceDirective =
  symbol "price" >> Price <$> commodityName <*> decimal <*> commodityName

datedDirective :: Parser DatedDirective
datedDirective =
  transactionDirective <|> openDirective <|> closeDirective <|> balanceDirective <|>
  priceDirective

configDirectiveInclude :: Parser ConfigDirective
configDirectiveInclude = symbol "include" >> Include . unpack <$> quotedString

configDirectiveOption :: Parser ConfigDirective
configDirectiveOption =
  symbol "option" >> Option <$> quotedString <*> quotedString

configDirective :: Parser ConfigDirective
configDirective = configDirectiveInclude <|> configDirectiveOption

directive :: Parser Directive
directive = Dated <$> date <*> datedDirective <|> Config <$> configDirective

eol :: Parser ()
eol = void $ token newline

comment :: Parser ()
comment = void (oneOf ";#" >> anyChar `manyTill` try eol)

block :: Parser a -> Parser a
block p = p `surroundedBy` many (comment <|> eol)

directives :: Parser [Directive]
directives = many (block directive) <* eof

parse' :: FilePath -> Text -> Either ParseError [Directive]
parse' = parse directives

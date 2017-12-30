module Parser
  ( parse
  , ParseException
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Decimal (Decimal)
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Text.Parsec
       (Parsec, (<|>), alphaNum, anyChar, between, char, count, digit,
        eof, letter, many, many1, manyTill, newline, noneOf, oneOf, option,
        optionMaybe, sepBy, sepBy1, string, try)
import qualified Text.Parsec as P
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

transaction :: Parser Transaction
transaction =
  Transaction <$> date <*> flag <*> quotedString <*> many tag <*>
  many1 (try posting)

open :: Parser Open
open =
  Open <$> date <* symbol "open" <*> accountName <*>
  commodityName `sepBy` symbol ","

close :: Parser Close
close = Close <$> date <* symbol "close" <*> accountName

balance :: Parser Balance
balance =
  Balance <$> date <* symbol "balance" <*> accountName <*> decimal <*>
  commodityName

price :: Parser Price
price =
  Price <$> date <* symbol "price" <*> commodityName <*> decimal <*>
  commodityName

include :: Parser Include
include = symbol "include" >> Include . unpack <$> quotedString

config :: Parser Option
config = symbol "option" >> Option <$> quotedString <*> quotedString

directive :: Parser (Directive P.SourcePos)
directive =
  (Opn <$> try open <|> Cls <$> try close <|> Trn <$> try transaction <|>
   Prc <$> try price <|>
   Bal <$> try balance <|>
   Inc <$> include <|>
   Opt <$> config) <*>
  P.getPosition

eol :: Parser ()
eol = void $ token newline

comment :: Parser ()
comment = void (oneOf ";#" >> anyChar `manyTill` try eol)

block :: Parser a -> Parser a
block p = p `surroundedBy` many (comment <|> eol)

directives :: Parser [Directive P.SourcePos]
directives = many (block directive) <* eof

parse :: (MonadThrow m) => FilePath -> Text -> m [Directive P.SourcePos]
parse f t =
  case P.parse directives f t of
    Left e -> throwM $ ParseException e
    Right d -> return d

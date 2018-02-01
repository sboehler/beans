module Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Account (AccountName(..))
import Data.Amount (Amount(..))
import Data.Commodity (CommodityName(..))
import Data.Decimal (Decimal)
import Data.Functor.Identity (Identity)
import Data.Lot (Lot(..))
import Data.Posting (Posting(..), PostingPrice(..))
import Data.Text.Lazy (Text, cons, pack, unpack)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Transaction (Flag(..), Tag(..), Transaction(..))
import Parser.AST
       (Balance(..), Close(..), Directive(..), Include(..), Open(..),
        Option(..), ParseException(..), PostingDirective(..), Price(..))
import Parser.Interpreter (completePostings)
import Text.Parsec
       ((<|>), alphaNum, anyChar, between, char, count, digit, eof,
        letter, many, many1, manyTill, newline, noneOf, oneOf, optionMaybe,
        sepBy, string, try)
import qualified Text.Parsec as P
import Text.Parsec.Number (fractional2, sign)

type Parser = P.ParsecT Text () Identity

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

amount :: Parser (Amount Decimal)
amount = Amount <$> decimal <*> commodityName

postingPriceUnit :: Parser (PostingPrice Decimal)
postingPriceUnit = symbol "@" >> UnitPrice <$> amount

postingPriceTotal :: Parser (PostingPrice Decimal)
postingPriceTotal = symbol "@@" >> TotalPrice <$> amount

postingPrice :: Parser (PostingPrice Decimal)
postingPrice = try postingPriceUnit <|> try postingPriceTotal

lot :: Day -> Parser (Lot Decimal)
lot d = braces $ Lot <$> amount <*> date' <*> label'
  where
    date' = try (symbol "," >> date) <|> pure d
    label' = optionMaybe (symbol "," >> quotedString)

posting :: Day -> Parser (Posting Decimal)
posting d =
  Posting <$> accountName <*> decimal <*> commodityName <*>
  optionMaybe postingPrice <*>
  optionMaybe (lot d)

postingDirective :: Day -> Parser PostingDirective
postingDirective d =
  newline >> symbol " " >>
  (try (CompletePosting <$> posting d) <|> try (WildcardPosting <$> accountName))

flagIncomplete :: Parser Flag
flagIncomplete = Incomplete <$ symbol "!"

flagComplete :: Parser Flag
flagComplete = Complete <$ symbol "*"

flag :: Parser Flag
flag = flagComplete <|> flagIncomplete

tag :: Parser Tag
tag = Tag <$> (cons <$> hash <*> text alphaNum)

transaction :: Parser Transaction
transaction = do
  d <- date
  f <- flag
  desc <- quotedString
  t <- many tag
  postings <- completePostings <$> many1 (try $ postingDirective d)
  case postings of
    Left err -> P.unexpected $ show err
    Right p -> return $ Transaction d f desc t p

open :: Parser Open
open =
  Open <$> date <* symbol "open" <*> accountName <*>
  commodityName `sepBy` symbol ","

close :: Parser Close
close = Close <$> date <* symbol "close" <*> accountName

balance :: Parser Balance
balance = Balance <$> date <* symbol "balance" <*> accountName <*> amount

price :: Parser Price
price = Price <$> date <* symbol "price" <*> commodityName <*> amount

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

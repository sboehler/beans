module Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Accounts (AccountName(..))
import Data.Amount (Amount(..))
import Data.Commodity (CommodityName(..))
import Data.Date (Date, fromGregorian)
import Data.Decimal (Decimal)
import Data.Functor.Identity (Identity)
import Data.Price (Price(..))
import Data.Text.Lazy (Text, cons, pack, unpack)
import Parser.AST
       (Balance(..), Close(..), Directive(..), Flag(..), Include(..),
        Open(..), Option(..), ParseException(..), Posting(..),
        PostingCost(..), PostingDirective(..), PostingPrice(..),
        PriceDirective(..), Tag(..), Transaction(..))
import Parser.Interpreter (completePostings)
import Text.Parsec
       ((<|>), alphaNum, anyChar, between, char, count, digit, eof,
        letter, many, many1, manyTill, newline, noneOf, oneOf, option,
        optionMaybe, sepBy, sepBy1, string, try)
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

date :: Parser Date
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

postingPriceUnit :: CommodityName -> Parser PostingPrice
postingPriceUnit c = symbol "@" >> UnitPrice <$> price c

postingPriceTotal :: Parser PostingPrice
postingPriceTotal = symbol "@@" >> TotalAmount <$> amount

postingPrice :: CommodityName -> Parser PostingPrice
postingPrice c = try (postingPriceUnit c) <|> try postingPriceTotal

postingCostDate :: Parser PostingCost
postingCostDate = PostingCostDate <$> date

postingCostAmount :: CommodityName -> Parser PostingCost
postingCostAmount commodity = PostingCostAmount <$> price commodity

postingCostLabel :: Parser PostingCost
postingCostLabel = PostingCostLabel <$> quotedString

postingCostElement :: CommodityName -> Parser PostingCost
postingCostElement commodity =
  try postingCostLabel <|> try postingCostDate <|>
  try (postingCostAmount commodity)

postingCost :: CommodityName -> Parser [PostingCost]
postingCost commodity =
  braces (postingCostElement commodity `sepBy1` symbol ",")

posting :: Parser Posting
posting = do
  account' <- accountName
  amount' <- amount
  cost' <- option [] $ postingCost (_commodity amount')
  price' <- optionMaybe $ postingPrice (_commodity amount')
  return $ Posting account' amount' cost' price'

postingDirective :: Parser PostingDirective
postingDirective =
  newline >> symbol " " >>
  (try (CompletePosting <$> posting) <|> try (WildcardPosting <$> accountName))

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
  postings <- completePostings <$> many1 (try postingDirective)
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

price :: CommodityName -> Parser (Price Decimal)
price c = Price c <$> amount

priceDirective :: Parser PriceDirective
priceDirective = do
  date' <- date
  _ <- symbol "price"
  commodity <- commodityName
  PriceDirective date' <$> price commodity

include :: Parser Include
include = symbol "include" >> Include . unpack <$> quotedString

config :: Parser Option
config = symbol "option" >> Option <$> quotedString <*> quotedString

directive :: Parser (Directive P.SourcePos)
directive =
  (Opn <$> try open <|> Cls <$> try close <|> Trn <$> try transaction <|>
   Prc <$> try priceDirective <|>
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

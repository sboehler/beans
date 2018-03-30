module Parser2 where

import           Control.Monad (void)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Account (AccountName (..))
import           Data.Amount (Amount (..))
import           Data.Char (isAlphaNum)
import           Data.Commodity (CommodityName (..))
import           Data.Lot (Lot (..))
import           Data.Posting (Posting (..), PostingPrice (..))
import           Data.Scientific (Scientific)
import           Data.Text.Lazy (Text, cons, unpack)
import           Data.Time.Calendar (Day, fromGregorian)
import           Data.Transaction           (Flag (..), Tag (..),
                                             Transaction (..))
import           Data.Void (Void)
import           Parser.AST                 (Balance (..), Close (..),
                                             Directive (..), Include (..),
                                             Open (..), Option (..),
                                             PostingDirective (..), Price (..))
import           Parser.Interpreter (completePostings)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "*" <|> L.skipLineComment "#" <|> L.skipLineComment ";"

scn ::  Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

readInt :: (Read a) => Int -> Parser a
readInt n = read <$> count n digitChar

date :: Parser Day
date =
  lexeme $
  fromGregorian <$> readInt 4 <* symbol "-" <*> readInt 2 <* symbol "-" <*> readInt 2

accountName :: Parser AccountName
accountName = lexeme $ AccountName <$> accountNameSegment `sepBy` symbol ":"
  where
    accountNameSegment =
      cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

commodityName :: Parser CommodityName
commodityName = lexeme $ CommodityName <$> takeWhileP (Just "alphanumeric") isAlphaNum

number :: Parser Scientific
number = lexeme (L.signed sc L.scientific)

amount :: Parser Amount
amount = Amount <$> number <*> commodityName

postingPrice :: Parser PostingPrice
postingPrice = try postingPriceUnit <|> try postingPriceTotal
  where
    postingPriceTotal = symbol "@@" >> TotalPrice <$> amount
    postingPriceUnit = symbol "@" >> UnitPrice <$> amount

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lot :: Day -> Parser Lot
lot d = braces $ Lot <$> amount <*> date' <*> label'
  where
    date' = try (symbol "," >> date) <|> pure d
    label' = optional (symbol "," >> quotedString)

quotedString :: Parser Text
quotedString =
  lexeme $ between (char '"') (char '"') (takeWhileP (Just "no quote") (/= '"'))

posting :: Day -> Parser Posting
posting d =
  Posting <$> accountName <*> number <*> commodityName <*>
  optional postingPrice <*>
  optional (lot d)


flag :: Parser Flag
flag = flagComplete <|> flagIncomplete
  where
    flagComplete = Complete <$ symbol "*"
    flagIncomplete = Incomplete <$ symbol "!"

tag :: Parser Tag
tag = Tag <$> (cons <$> char '#' <*> takeWhile1P (Just "alphanum") isAlphaNum)

postingDirective :: Day -> Parser PostingDirective
postingDirective d =
  try (CompletePosting <$> posting d) <|> try (WildcardPosting <$> accountName)

transaction :: Parser Transaction
transaction = L.indentBlock scn p
  where
    p = do
      d <- date
      f <- flag
      desc <- quotedString
      t <- many tag
      let fn a = do
            let result = completePostings a
            case result of
              Left err -> fail $ show err
              Right postings -> return $ Transaction d f desc t postings
      return $ L.IndentSome Nothing fn (postingDirective d)

open :: Parser Open
open =
  Open <$> date <* symbol "open" <*> accountName <*>
  (commodityName `sepBy` symbol ",") <* scn

close :: Parser Close
close = Close <$> date <* symbol "close" <*> accountName <* scn

balance :: Parser Balance
balance = Balance <$> date <* symbol "balance" <*> accountName <*> amount <* scn

price :: Parser Price
price = Price <$> date <* symbol "price" <*> commodityName <*> amount <* scn

include :: Parser Include
include = symbol "include" >> Include . unpack <$> quotedString <* scn

config :: Parser Option
config = symbol "option" >> Option <$> quotedString <*> quotedString <* scn

directive :: Parser (Directive SourcePos)
directive =
  (Opn <$> try open <|> Cls <$> try close <|> Prc <$> try price <|>
   Bal <$> try balance <|>
   Inc <$> try include <|>
   Opt <$> try config <|>
   Trn <$> try transaction) <*>
  getPosition

directives :: Parser [Directive SourcePos]
directives = many (L.nonIndented scn directive) <* eof

parse' :: (MonadThrow m) => FilePath -> Text -> m [Directive SourcePos]
parse' f t = case parse directives f t of
    Left e  -> throwM e
    Right d -> return d

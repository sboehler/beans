module Parser where

import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Data.Char                  (isAlphaNum)
import           Data.Scientific            (Scientific)
import           Data.Text.Lazy             (Text, cons, unpack)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Void                  (Void)
import           Parser.AST
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

account :: Parser AccountName
account = lexeme $ AccountName <$> segment `sepBy` symbol ":"
  where
    segment =
      cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

commodity :: Parser CommodityName
commodity = lexeme $ CommodityName <$> takeWhileP (Just "alphanumeric") isAlphaNum

number :: Parser Scientific
number = lexeme (L.signed sc L.scientific)

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lot :: Day -> Parser Lot
lot d = braces $ Lot <$> number <*> commodity <*> date' <*> label'
  where
    date' = (symbol "," >> date) <|> pure d
    label' = optional (symbol "," >> quotedString)

quotedString :: Parser Text
quotedString =
  lexeme $ between (char '"') (char '"') (takeWhileP (Just "no quote") (/= '"'))

posting :: Day -> Parser Posting
posting d = do
  a <- account
  Posting a <$> number <*> commodity <*> optional (lot d) <|> return (Wildcard a)

flag :: Parser Flag
flag = flagComplete <|> flagIncomplete
  where
    flagComplete = Complete <$ symbol "*"
    flagIncomplete = Incomplete <$ symbol "!"

tag :: Parser Tag
tag = Tag <$> (cons <$> char '#' <*> takeWhile1P (Just "alphanum") isAlphaNum)

transaction :: Parser Transaction
transaction =
  L.indentBlock scn $ do
    d <- date
    f <- flag
    desc <- quotedString
    t <- many tag
    return $
      L.IndentSome Nothing (return . Transaction d f desc t) (posting d)

open :: Parser Open
open =
  Open <$> date <* symbol "open" <*> account <*>
  (commodity `sepBy` symbol ",") <* scn

close :: Parser Close
close = Close <$> date <* symbol "close" <*> account <* scn

balance :: Parser Balance
balance = Balance <$> date <* symbol "balance" <*> account <*> number <*> commodity <* scn

price :: Parser Price
price = Price <$> date <* symbol "price" <*> commodity <*> number <*> commodity <* scn

include :: Parser Include
include = symbol "include" >> Include . unpack <$> quotedString <* scn

config :: Parser Option
config = symbol "option" >> Option <$> quotedString <*> quotedString <* scn

directive :: Parser (Directive SourcePos)
directive =
  (Trn <$> try transaction <|> Opn <$> try open <|> Cls <$> try close <|> Prc <$> try price <|>
   Bal <$> try balance <|>
   Inc <$> include <|>
   Opt <$> config) <*>
  getPosition

directives :: Parser [Directive SourcePos]
directives = many (L.nonIndented scn directive) <* eof

parse' :: (MonadThrow m) => FilePath -> Text -> m [Directive SourcePos]
parse' f t = case parse directives f t of
    Left e  -> throwM e
    Right d -> return d

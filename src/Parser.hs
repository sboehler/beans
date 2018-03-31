module Parser where

import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Data.Char                  (isAlphaNum)
import           Data.Functor               (($>))
import           Data.Scientific            (Scientific)
import           Data.Text.Lazy             (Text, cons, unpack)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Void                  (Void)
import           Parser.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Pos        as P

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment =
  L.skipLineComment "*" <|> L.skipLineComment "#" <|> L.skipLineComment ";"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

date :: Parser Day
date =
  lexeme $ fromGregorian <$> digits 4 <* dash <*> digits 2 <* dash <*> digits 2
  where
    dash = symbol "-"
    digits n = read <$> count n digitChar

account :: Parser AccountName
account = lexeme $ AccountName <$> segment `sepBy` colon
  where
    segment = cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum
    colon = symbol ":"

commodity :: Parser CommodityName
commodity =
  lexeme $ CommodityName <$> takeWhileP (Just "alphanumeric") isAlphaNum

number :: Parser Scientific
number = lexeme $ L.signed sc L.scientific

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotedString :: Parser Text
quotedString =
  lexeme $ between quote quote (takeWhileP (Just "no quote") (/= '"'))
  where
    quote = char '"'

lot :: Day -> Parser Lot
lot d = braces $ Lot <$> number <*> commodity <*> lotDate <*> lotLabel
  where
    comma = symbol ","
    lotDate = (comma >> date) <|> pure d
    lotLabel = optional (comma >> quotedString)

postingPrice :: Parser ()
postingPrice = (symbol "@@" *> number *> commodity) $> ()

posting :: Day -> Parser Posting
posting d = do
  a <- account
  Posting a <$> number <*> commodity <*> optional (lot d) <* optional postingPrice <|>
    return (Wildcard a)

flag :: Parser Flag
flag = complete <|> incomplete
  where
    complete = Complete <$ symbol "*"
    incomplete = Incomplete <$ symbol "!"

tag :: Parser Tag
tag = Tag <$> (cons <$> char '#' <*> takeWhile1P (Just "alphanum") isAlphaNum)

transaction :: Day -> Parser Event
transaction d = do
  f <- flag
  desc <- quotedString
  t <- many tag
  indent <- L.indentGuard scn GT P.pos1
  p <- some $ try (L.indentGuard scn EQ indent *> posting d)
  return $ Trn d $ Transaction f desc t p

open :: Day -> Parser Event
open d =
  Opn d <$>
  (Open <$ symbol "open" <*> account <*> (commodity `sepBy` symbol ","))

close :: Day -> Parser Event
close d = Cls d <$> (Close <$ symbol "close" <*> account)

balance :: Day -> Parser Event
balance d =
  Bal d <$> (Balance <$ symbol "balance" <*> account <*> number <*> commodity)

price :: Day -> Parser Event
price d =
  Prc d <$> (Price <$ symbol "price" <*> commodity <*> number <*> commodity)

event :: Parser Event
event =
  date >>= \d -> transaction d <|> open d <|> close d <|> balance d <|> price d

include :: Parser Include
include = symbol "include" >> Include . unpack <$> quotedString

config :: Parser Option
config = symbol "option" >> Option <$> quotedString <*> quotedString

directive :: Parser (Directive SourcePos)
directive =
  L.nonIndented scn $
  (Evt <$> event <|> Inc <$> include <|> Opt <$> config) <*> getPosition <* scn

directives :: Parser [Directive SourcePos]
directives = some directive <* eof

parseFile :: (MonadThrow m) => FilePath -> Text -> m [Directive SourcePos]
parseFile f t =
  case parse directives f t of
    Left e  -> throwM e
    Right d -> return d

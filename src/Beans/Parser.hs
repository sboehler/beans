module Beans.Parser where

import           Beans.Data.Accounts        (AccountName (..), AccountType (..),
                                             Amount, CommodityName (..),
                                             Lot (..))
import           Beans.Data.Directives      (Balance (..), Close (..),
                                             Directive (..), Flag (..),
                                             Include (..), Open (..),
                                             Option (..), Posting (..),
                                             Price (..), Tag (..),
                                             Transaction (..),
                                             mkBalancedTransaction)
import           Beans.Data.Restrictions    (Restriction (..))
import           Control.Monad              (void)
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans        (liftIO)
import           Data.Char                  (isAlphaNum)
import           Data.Functor               (($>))
import           Data.Monoid                (Sum (Sum))
import qualified Data.Set                   as S
import           Data.Text                  (Text, cons, unpack)
import           Data.Text.IO               (readFile)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Prelude                    hiding (readFile)
import           System.FilePath.Posix      (combine, takeDirectory)
import           Text.Megaparsec            (ErrorFancy (..), Parsec,
                                             ShowErrorComponent (..), between,
                                             count, empty, eof, fancyFailure,
                                             getPosition, many, optional, parse,
                                             parseErrorPretty, sepBy, sepBy1,
                                             some, takeWhile1P, takeWhileP, try,
                                             (<|>))
import           Text.Megaparsec.Char       (char, digitChar, letterChar,
                                             space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Pos        as P

-- The exception exported by this module
newtype ParserException =
  ParserException String
  deriving (Eq)

instance Show ParserException where
  show (ParserException s) = s

instance Exception ParserException

-- Internal exception to indicate unbalanced transactions
newtype VerifyException =
  UnbalancedTransaction P.SourcePos
  deriving (Eq, Show, Ord)

instance Exception VerifyException

instance ShowErrorComponent VerifyException where
  showErrorComponent (UnbalancedTransaction pos) =
    "Unbalanced transaction: " ++ show pos

verifyError :: P.SourcePos -> Parser a
verifyError = fancyFailure . S.singleton . ErrorCustom . UnbalancedTransaction

-- The parser type
type Parser = Parsec VerifyException Text

-- parsers
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

identifier :: Parser Text
identifier = cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

accountType :: Parser AccountType
accountType = read . unpack <$> identifier

account :: Parser AccountName
account =
  lexeme $ AccountName <$> accountType <* colon <*> identifier `sepBy` colon
  where
    colon = symbol ":"

commodity :: Parser CommodityName
commodity = lexeme $ CommodityName <$> identifier

amount :: Parser Amount
amount = lexeme $ Sum <$> L.signed sc L.scientific

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotedString :: Parser Text
quotedString =
  lexeme $ between quote quote (takeWhileP (Just "no quote") (/= '"'))
  where
    quote = char '"'

lot :: Day -> Parser Lot
lot d = braces (Lot <$> amount <*> commodity <*> lotDate <*> lotLabel)
  where
    comma = symbol ","
    lotDate = (comma >> date) <|> pure d
    lotLabel = optional (comma >> quotedString)

postingPrice :: Parser ()
postingPrice = (at *> optional at *> amount *> commodity) $> ()
  where
    at = symbol "@"

posting :: Day -> Parser Posting
posting d =
  Posting <$> (Just <$> getPosition) <*> account <*> amount <*> commodity <*>
  optional (lot d) <*
  optional postingPrice

flag :: Parser Flag
flag = complete <|> incomplete
  where
    complete = Complete <$ symbol "*"
    incomplete = Incomplete <$ symbol "!"

tag :: Parser Tag
tag = Tag <$> (cons <$> char '#' <*> takeWhile1P (Just "alphanum") isAlphaNum)

transaction :: P.SourcePos -> Day -> Parser Transaction
transaction pos d = do
  f <- flag
  desc <- quotedString
  t <- many tag
  indent <- L.indentGuard scn GT P.pos1
  p <- some $ try (L.indentGuard scn EQ indent *> posting d)
  w <- optional $ try (L.indentGuard scn EQ indent *> account)
  case mkBalancedTransaction (Just pos) d f desc t p w of
    Just t' -> return t'
    Nothing -> verifyError pos

open :: P.SourcePos -> Day -> Parser Open
open pos d = Open (Just pos) d <$ symbol "open" <*> account <*> restriction

restriction :: Parser Restriction
restriction =
  RestrictedTo <$> (commodity `sepBy1` symbol ",") <|> return NoRestriction

close :: P.SourcePos -> Day -> Parser Close
close pos d = Close (Just pos) d <$ symbol "close" <*> account

balance :: P.SourcePos -> Day -> Parser Balance
balance pos d =
  Balance (Just pos) d <$ symbol "balance" <*> account <*> amount <*> commodity

price :: P.SourcePos -> Day -> Parser Price
price pos d = Price pos d <$ symbol "price" <*> commodity <*> p <*> commodity
  where
    p = lexeme $ L.signed sc L.scientific

event :: Parser Directive
event = do
  pos <- getPosition
  d <- date
  Trn <$> transaction pos d <|> Opn <$> open pos d <|> Cls <$> close pos d <|>
    Bal <$> balance pos d <|>
    Prc <$> price pos d

include :: Parser Include
include =
  symbol "include" >> Include <$> getPosition <*> (unpack <$> quotedString)

config :: Parser Option
config =
  symbol "option" >> Option <$> getPosition <*> quotedString <*> quotedString

directive :: Parser Directive
directive =
  L.nonIndented scn $ (event <|> Inc <$> include <|> Opt <$> config) <* scn

directives :: Parser [Directive]
directives = some directive <* eof

parseSource :: (MonadThrow m) => FilePath -> Text -> m [Directive]
parseSource f t =
  case parse directives f t of
    Left e  -> (throwM . ParserException . parseErrorPretty) e
    Right d -> return d

getIncludedFiles :: FilePath -> [Directive] -> [FilePath]
getIncludedFiles fp ast =
  [combine (takeDirectory fp) path | (Inc (Include _ path)) <- ast]

parseFile :: (MonadIO m, MonadThrow m) => FilePath -> m [Directive]
parseFile filePath = do
  source <- liftIO $ readFile filePath
  ast <- parseSource filePath source
  asts <- concat <$> traverse parseFile (getIncludedFiles filePath ast)
  return $ ast ++ asts

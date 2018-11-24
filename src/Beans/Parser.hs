module Beans.Parser where

import           Beans.Model                    ( Account(..)
                                                , AccountType(..)
                                                , Amount
                                                , Amounts
                                                , Date
                                                , fromGreg
                                                , Commodity(..)
                                                , Lot(..)
                                                , Position(..)
                                                , Command(..)
                                                , Dated(..)
                                                , Directive(..)
                                                , Flag(..)
                                                , Include(..)
                                                , Option(..)
                                                , Restriction(..)
                                                , Tag(..)
                                                , mkBalancedTransaction
                                                )
import qualified Beans.Data.Map                as M

import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans            ( liftIO )
import           Data.Char                      ( isAlphaNum )
import           Data.Functor                   ( ($>) )
import           Data.Monoid                    ( Sum(Sum) )
import qualified Data.Set                      as S
import           Data.Text                      ( Text
                                                , cons
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Prelude                 hiding ( readFile )
import           System.FilePath.Posix          ( combine
                                                , takeDirectory
                                                )
import           Text.Megaparsec                ( ErrorFancy(..)
                                                , Parsec
                                                , ShowErrorComponent(..)
                                                , between
                                                , count
                                                , empty
                                                , eof
                                                , fancyFailure
                                                , getPosition
                                                , many
                                                , optional
                                                , parse
                                                , parseErrorPretty
                                                , sepBy
                                                , sepBy1
                                                , some
                                                , takeWhile1P
                                                , takeWhileP
                                                , try
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( char
                                                , digitChar
                                                , letterChar
                                                , space1
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Text.Megaparsec.Pos           as P

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
  where f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pDate :: Parser Date
pDate = lexeme $ fromGreg <$> digits 4 <* dash <*> digits 2 <* dash <*> digits
  2
 where
  dash = symbol "-"
  digits n = read <$> count n digitChar

identifier :: Parser Text
identifier =
  cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

accountType :: Parser AccountType
accountType = read . unpack <$> identifier

account :: Parser Account
account =
  lexeme $ Account <$> accountType <* colon <*> identifier `sepBy` colon
  where colon = symbol ":"

commodity :: Parser Commodity
commodity = lexeme $ Commodity <$> identifier

amount :: Parser Amount
amount = lexeme $ Sum <$> L.signed sc L.scientific

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotedString :: Parser Text
quotedString = lexeme
  $ between quote quote (takeWhileP (Just "no quote") (/= '"'))
  where quote = char '"'

lot :: Date -> Parser Lot
lot d = braces (Lot <$> amount <*> commodity <*> lotDate <*> lotLabel)
 where
  comma    = symbol ","
  lotDate  = (comma >> pDate) <|> pure d
  lotLabel = optional (comma >> quotedString)

postingPrice :: Parser ()
postingPrice = (at *> optional at *> amount *> commodity) $> ()
  where at = symbol "@"

posting :: Date -> Parser (Position, Amounts)
posting d = do
  a <- account
  s <- amount
  c <- commodity
  l <- optional (lot d)
  _ <- optional postingPrice
  let s' = M.singleton c s
  return (Position a c l, s')


flag :: Parser Flag
flag = complete <|> incomplete
 where
  complete   = Complete <$ symbol "*"
  incomplete = Incomplete <$ symbol "!"

tag :: Parser Tag
tag = Tag <$> (cons <$> char '#' <*> takeWhile1P (Just "alphanum") isAlphaNum)

transaction :: Date -> Parser Command
transaction d = do
  f      <- flag
  desc   <- quotedString
  t      <- many tag
  indent <- L.indentGuard scn GT P.pos1
  p      <- some $ try (L.indentGuard scn EQ indent *> posting d)
  w      <- optional $ try (L.indentGuard scn EQ indent *> account)
  case mkBalancedTransaction f desc t (M.fromListM p) w of
    Just t' -> return t'
    Nothing -> do
      pos <- getPosition
      verifyError pos

open :: Parser Command
open = Open <$ symbol "open" <*> account <*> restriction

restriction :: Parser Restriction
restriction =
  RestrictedTo <$> (commodity `sepBy1` symbol ",") <|> return NoRestriction

close :: Parser Command
close = Close <$ symbol "close" <*> account

balance :: Parser Command
balance = Balance <$ symbol "balance" <*> account <*> amount <*> commodity

price :: Parser Command
price = Price <$ symbol "price" <*> commodity <*> p <*> commodity
  where p = lexeme $ L.signed sc L.scientific

command :: Date -> Parser Command
command d = transaction d <|> open <|> close <|> balance <|> price

include :: Parser Include
include =
  symbol "include" >> Include <$> getPosition <*> (unpack <$> quotedString)

config :: Parser Option
config =
  symbol "option" >> Option <$> getPosition <*> quotedString <*> quotedString

commandDirective :: Parser Directive
commandDirective = do
  d <- try pDate
  DatedCommandDirective . Dated d <$> command d

directive :: Parser Directive
directive =
  L.nonIndented scn
    $  (   IncludeDirective
       <$> include
       <|> OptionDirective
       <$> config
       <|> commandDirective
       )
    <* scn

directives :: Parser [Directive]
directives = some directive <* try (scn >> eof)

parseSource :: (MonadThrow m) => FilePath -> Text -> m [Directive]
parseSource f t = case parse directives f t of
  Left  e -> (throwM . ParserException . parseErrorPretty) e
  Right d -> return d

getIncludedFiles :: FilePath -> [Directive] -> [FilePath]
getIncludedFiles fp ast =
  [ combine (takeDirectory fp) path
  | (IncludeDirective (Include _ path)) <- ast
  ]

parseFile :: (MonadIO m, MonadThrow m) => FilePath -> m [Directive]
parseFile filePath = do
  ast  <- liftIO (readFile filePath) >>= parseSource filePath
  asts <- concat <$> traverse parseFile (getIncludedFiles filePath ast)
  return $ ast ++ asts

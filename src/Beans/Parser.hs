module Beans.Parser where

import           Beans.Model                    ( Account(..)
                                                , Amount
                                                , Amounts
                                                , Date
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
                                                , Open(..)
                                                , Close(..)
                                                , Balance(..)
                                                , Transaction(..)
                                                , Price(..)
                                                )
import qualified Beans.Data.Map                as M
import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans            ( liftIO )
import           Data.Void                      ( Void )
import           Data.Char                      ( isAlphaNum )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text
                                                , cons
                                                , pack
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Prelude                 hiding ( readFile )
import           System.FilePath.Posix          ( combine
                                                , takeDirectory
                                                )
import           Beans.Megaparsec               ( Parsec
                                                , between
                                                , empty
                                                , eof
                                                , getPosition
                                                , many
                                                , manyTill
                                                , optional
                                                , parse
                                                , parseErrorPretty
                                                , parseAmount
                                                , parseISODate
                                                , parseAccount
                                                , parseCommodity
                                                , parseDecimal
                                                , sepBy1
                                                , some
                                                , takeWhile1P
                                                , try
                                                , (<|>)
                                                , char
                                                , anyChar
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

-- The parser type
type Parser = Parsec Void Text

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

account :: Parser Account
account = lexeme parseAccount

commodity :: Parser Commodity
commodity = lexeme parseCommodity

amount :: Parser Amount
amount = lexeme $ parseAmount sc

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotedString :: Parser Text
quotedString = lexeme $ quote >> t
 where
  quote = char '"'
  t     = pack <$> manyTill ((char '\\' >> anyChar) <|> anyChar) quote

lot :: Date -> Parser Lot
lot d = braces (Lot <$> amount <*> commodity <*> lotDate <*> lotLabel)
 where
  comma    = symbol ","
  lotDate  = (comma >> lexeme parseISODate) <|> pure d
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

transaction :: Date -> Parser Transaction
transaction d = do
  f      <- flag
  desc   <- quotedString
  t      <- many tag
  indent <- L.indentGuard scn GT P.pos1
  p      <- some $ try (L.indentGuard scn EQ indent *> posting d)
  w      <- optional $ try (L.indentGuard scn EQ indent *> account)
  case mkBalancedTransaction f desc t (M.fromListM p) w of
    Just t' -> return t'
    Nothing -> fail "Unbalanced transaction"


open :: Parser Open
open = Open <$ symbol "open" <*> account <*> restriction

restriction :: Parser Restriction
restriction =
  RestrictedTo <$> (commodity `sepBy1` symbol ",") <|> return NoRestriction

close :: Parser Close
close = Close <$ symbol "close" <*> account

balance :: Parser Balance
balance = Balance <$ symbol "balance" <*> account <*> amount <*> commodity

price :: Parser Price
price = Price <$ symbol "price" <*> commodity <*> p <*> commodity
  where p = lexeme $ parseDecimal sc

command :: Date -> Parser Command
command d =
  CmdTransaction
    <$> transaction d
    <|> CmdOpen
    <$> open
    <|> CmdClose
    <$> close
    <|> CmdBalance
    <$> balance
    <|> CmdPrice
    <$> price

include :: Parser Include
include =
  symbol "include" >> Include <$> getPosition <*> (unpack <$> quotedString)

config :: Parser Option
config =
  symbol "option" >> Option <$> getPosition <*> quotedString <*> quotedString

commandDirective :: Parser Directive
commandDirective = do
  d <- try $ lexeme parseISODate
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

module Beans.Parser
  ( Parser,
    parseFile,
    directives,
    parseSource,
    ParserException,
    parsePrices,
  )
where

import Beans.Account (Account (..))
import Beans.Amount (Amount)
import Beans.Assertion (Assertion (Assertion))
import Beans.Close (Close (Close))
import Beans.Command (Command (..), Directive (..))
import Beans.Commodity (Commodity)
import Beans.Date (Date)
import qualified Beans.Date as Date
import Beans.Include (Include (Include))
import Beans.Lot (Lot (Lot))
import qualified Beans.Megaparsec as M
import Beans.Open (Open (Open))
import Beans.Option (Option (Option))
import Beans.Price (Price (Price))
import Beans.Transaction (Posting (..), Tag (..), Transaction)
import qualified Beans.Transaction as Transaction
import Beans.ValAmount (ValAmount)
import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Traversable (for)
import Data.Void (Void)
import qualified System.FilePath.Posix as Posix
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

-- The exception exported by this module
newtype ParserException
  = ParserException String
  deriving (Eq)

instance Show ParserException where
  show (ParserException s) = s

instance Exception ParserException

-- The parser type
type Parser = M.Parsec Void Text

-- API

getIncludedFiles :: FilePath -> [Directive] -> [FilePath]
getIncludedFiles fp ast =
  [ Posix.combine (Posix.takeDirectory fp) path
    | (IncludeDirective (Include _ path)) <- ast
  ]

parseFile :: (MonadIO m, MonadThrow m) => FilePath -> m [Directive]
parseFile filePath = do
  text <- Text.decodeUtf8 <$> (liftIO $ BS.readFile filePath)
  ast <- parseSource directives filePath text
  asts <- for (getIncludedFiles filePath ast) parseFile
  pure $ concat $ ast : asts

parsePrices :: (MonadIO m, MonadThrow m) => FilePath -> m [Price]
parsePrices filePath = do
  text <- Text.decodeUtf8 <$> (liftIO $ BS.readFile filePath)
  parseSource (M.optional scn *> M.some (price <* M.choice [scn, M.eof])) filePath text

-- parsers

parseSource :: (MonadThrow m) => Parser a -> FilePath -> Text -> m a
parseSource p f t = case M.parse p f t of
  Left e -> (throwM . ParserException . M.errorBundlePretty) e
  Right d -> pure d

directives :: Parser [Directive]
directives = M.optional scn *> directive `M.sepEndBy` scn <* M.eof

directive :: Parser Directive
directive =
  L.nonIndented sc $ M.choice [include, option, command]

scn :: Parser ()
scn = M.skipSome $ M.choice [M.space1, L.skipLineComment "*", L.skipLineComment "#", L.skipLineComment ";"]

sc :: Parser ()
sc = L.space (void $ M.takeWhile1P Nothing f) M.empty M.empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

account :: Parser Account
account = lexeme M.parseAccount

commodity :: Parser Commodity
commodity = lexeme M.parseCommodity

amount :: Parser ValAmount
amount = lexeme $ M.parseValAmount sc

decimal :: Parser Amount
decimal = lexeme $ M.parseAmount sc

braces :: Parser a -> Parser a
braces = M.between (symbol "{") (symbol "}")

quotedString :: Parser Text
quotedString = lexeme $ quote >> t
  where
    quote = M.char '"'
    t = Text.pack <$> M.manyTill (M.choice [M.char '\\' >> M.anySingle, M.anySingle]) quote

lot :: Date.Date -> Parser Lot
lot d = braces (Lot <$> decimal <*> commodity <*> lotDate <*> lotLabel)
  where
    comma = symbol ","
    lotDate = M.choice [comma >> date, pure d]
    lotLabel = M.optional (comma >> quotedString)

posting :: Date.Date -> Parser Posting
posting d = do
  a <- account
  s <- amount
  c <- commodity
  l <- M.optional (lot d)
  t <- M.optional tag
  pure $ Posting a c l s t

tag :: Parser Tag
tag = Tag <$> lexeme (Text.cons <$> M.char '#' <*> M.takeWhile1P (Just "alphanum") Char.isAlphaNum)

transaction :: Parser Transaction
transaction = do
  d <- date
  -- support legacy stars
  _ <- M.optional (symbol "*")
  desc <- quotedString
  t <- M.many tag
  -- tolerate indented postings
  p <- M.some (M.try (lexeme M.eol *> posting d))
  w <- M.optional $ M.try (lexeme M.eol *> account)
  case Transaction.createBalanced d desc t p w of
    Just t' -> pure t'
    Nothing -> fail "Unbalanced transaction"

open :: Parser Open
open = Open <$> date <*> (symbol "open" *> account)

close :: Parser Close
close = Close <$> date <*> (symbol "close" *> account)

balanceAssertion :: Parser Assertion
balanceAssertion = Assertion <$> date <*> (symbol "balance" *> account) <*> decimal <*> commodity

price :: Parser Price
price = Price <$> date <*> (symbol "price" *> commodity) <*> (Scientific.toRealFloat <$> lexeme L.scientific) <*> commodity

command :: Parser Directive
command = do
  start <- M.getOffset
  cmd <-
    M.choice
      [ M.try (CmdTransaction <$> transaction),
        M.try (CmdOpen <$> open),
        M.try (CmdClose <$> close),
        M.try (CmdAssertion <$> balanceAssertion),
        CmdPrice <$> price
      ]
  end <- M.getOffset
  pure $ CmdDirective (start, end) cmd

include :: Parser Directive
include =
  IncludeDirective <$> (symbol "include" >> Include <$> M.getSourcePos <*> (Text.unpack <$> quotedString))

option :: Parser Directive
option =
  OptionDirective <$> (symbol "option" >> Option <$> M.getSourcePos <*> quotedString <*> quotedString)

date :: Parser Date
date = lexeme M.parseISODate

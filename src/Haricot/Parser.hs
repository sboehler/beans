module Haricot.Parser where

import           Control.Monad              (void)
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans        (liftIO)
import           Data.Char                  (isAlphaNum)
import           Data.Functor               (($>))
import qualified Data.Map.Strict            as M
import           Data.Scientific            (Scientific)
import qualified Data.Set                   as S
import           Data.Text.Lazy             (Text, cons, unpack)
import           Data.Text.Lazy.IO          (readFile)
import           Data.Time.Calendar         (Day, fromGregorian)
import           Haricot.AST
import           Prelude                    hiding (readFile)
import           System.FilePath.Posix      (combine, takeDirectory)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Pos        as P

newtype VerifyException =
  UnbalancedTransaction P.SourcePos
  deriving (Eq, Show, Ord)

instance Exception VerifyException

instance ShowErrorComponent VerifyException where
  showErrorComponent (UnbalancedTransaction pos) =
    "Unbalanced transaction: " ++ show pos

verifyError :: P.SourcePos -> Parser a
verifyError = fancyFailure . S.singleton . ErrorCustom . UnbalancedTransaction

type Parser = Parsec VerifyException Text

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
    segment =
      cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum
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
postingPrice = (symbol "@" *> optional (symbol "@") *> number *> commodity) $> ()

data PostingDirective
  = CP Posting
  | WP WildcardPosting

data WildcardPosting = WildcardPosting
  { _pos     :: P.SourcePos
  , _account :: AccountName
  } deriving (Show, Eq)

posting :: Day -> P.SourcePos -> AccountName -> Parser Posting
posting d p a =
  Posting p a <$> number <*> commodity <*> optional (lot d) <*
  optional postingPrice

wildcardPosting :: P.SourcePos -> AccountName -> Parser WildcardPosting
wildcardPosting p a = return $ WildcardPosting p a

postingDirective :: Day -> Parser PostingDirective
postingDirective d = do
  pos <- getPosition
  a <- account
  CP <$> posting d pos a <|> WP <$> wildcardPosting pos a

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
  p <- some $ try (L.indentGuard scn EQ indent *> postingDirective d)
  let postings = completePostings pos p
  case postings of
    Left _ -> verifyError pos
    Right p' -> return $ Transaction pos d f desc t p'


open :: P.SourcePos -> Day -> Parser Open
open pos d = Open pos d <$ symbol "open" <*> account <*> restriction

restriction :: Parser Restriction
restriction =  RestrictedTo <$> (commodity `sepBy1` symbol ",") <|> return NoRestriction

close :: P.SourcePos -> Day -> Parser Close
close pos d = Close pos d <$ symbol "close" <*> account

balance :: P.SourcePos -> Day -> Parser Balance
balance pos d =
  Balance pos d <$ symbol "balance" <*> account <*> number <*> commodity

price :: P.SourcePos -> Day -> Parser Price
price pos d =
  Price pos d <$ symbol "price" <*> commodity <*> number <*> commodity

event :: Parser Directive
event = do
  pos <- getPosition
  d <- date
  Trn <$> transaction pos d <|> Opn <$> open pos d <|> Cls <$> close pos d <|>
    Bal <$> balance pos d <|>
    Prc <$> price pos d

include :: Parser Include
include = symbol "include" >> Include <$> getPosition <*> (unpack <$> quotedString)

config :: Parser Option
config = symbol "option" >> Option <$> getPosition <*> quotedString <*> quotedString

directive :: Parser Directive
directive =
  L.nonIndented scn $
  (event <|> Inc <$> include <|> Opt <$> config) <* scn

directives :: Parser [Directive ]
directives = some directive <* eof

parseSource :: (MonadThrow m) => FilePath -> Text -> m [Directive ]
parseSource f t =
  case parse directives f t of
    Left e  -> throwM e
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

completePostings :: (MonadThrow m) => P.SourcePos -> [PostingDirective] -> m [Posting]
completePostings pos p =
  let wildcards = [w | WP w <- p]
      postings = [c | CP c <- p]
   in case calculateImbalances postings of
        [] -> return postings
        imbalances ->
          case wildcards of
            [w] -> return $ postings ++ map (balanceImbalance w) imbalances
            _   -> throwM $ UnbalancedTransaction pos

balanceImbalance :: WildcardPosting -> (CommodityName, Scientific) -> Posting
balanceImbalance WildcardPosting {_pos, _account} (c, a) =
  Posting
    {_amount = negate a, _commodity = c, _lot = Nothing, ..}

calculateImbalances :: [Posting] -> [(CommodityName, Scientific)]
calculateImbalances =
  M.toList . M.filter ((> 0.005) . abs) . M.fromListWith (+) . fmap weight

weight :: Posting  -> (CommodityName, Scientific)
weight Posting {..} =
  case _lot of
    Just Lot {_price, _targetCommodity} ->
      (_targetCommodity, _amount * _price)
    _ -> (_commodity, _amount)

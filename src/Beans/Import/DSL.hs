{-# LANGUAGE GADTs #-}

module Beans.Import.DSL where

import           Beans.Model                              ( Account(..)
                                                          , AccountType
                                                          , Date
                                                          , fromGreg
                                                          , Amount
                                                          , Date
                                                          , Commodity
                                                          )
import           Control.Exception                        ( Exception )
import           Control.Monad                            ( msum )
import           Control.Monad.Catch                      ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( Reader
                                                          , asks
                                                          , runReaderT
                                                          )
import           Data.Bool                                ( bool )
import           Data.Char                                ( isAlphaNum )
import           Data.Functor.Identity                    ( runIdentity )
import           Data.Monoid                              ( Sum(Sum)
                                                          , (<>)
                                                          )
import           Data.Text                                ( Text
                                                          , cons
                                                          , pack
                                                          , unpack
                                                          )
import           Data.Text.IO                             ( readFile )
import           Data.Void                                ( Void )
import           Prelude                           hiding ( readFile )
import           Text.Megaparsec                          ( Parsec
                                                          , between
                                                          , choice
                                                          , count
                                                          , empty
                                                          , eof
                                                          , many
                                                          , parse
                                                          , parseErrorPretty
                                                          , sepBy
                                                          , takeWhileP
                                                          , try
                                                          , (<|>)
                                                          )
import           Text.Megaparsec.Char                     ( char
                                                          , digitChar
                                                          , letterChar
                                                          , space1
                                                          , string
                                                          )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Expr                     ( Operator
                                                            ( InfixL
                                                            , Prefix
                                                            )
                                                          , makeExprParser
                                                          )
import           Text.Regex.PCRE                          ( (=~) )

-- Abstract Syntax Tree
type Rules = [Rule]

data Rule =
  Rule (E Bool)
       Account
  deriving (Show)

data E a where
  EVarAmount :: E Amount
  EVarDescription :: E Text
  EVarType :: E Text
  EVarDate :: E Date
  EVarImporter :: E Text
  EDate :: Date -> E Date
  EAmount :: Amount -> E Amount
  EText :: Text -> E Text
  EBool :: Bool -> E Bool
  EAnd :: E Bool -> E Bool -> E Bool
  EOr :: E Bool -> E Bool -> E Bool
  ENot :: E Bool -> E Bool
  EPlus :: Num a => E a -> E a -> E a
  EMinus :: Num a => E a -> E a -> E a
  EAbs :: Num a => E a -> E a
  ELT :: (Show a, Ord a) => E a -> E a -> E Bool
  ELE :: (Show a, Ord a) => E a -> E a -> E Bool
  EEQ :: (Show a, Eq a) => E a -> E a -> E Bool
  EGE :: (Show a, Ord a) => E a -> E a -> E Bool
  EGT :: (Show a, Ord a) => E a -> E a -> E Bool
  ENE :: (Show a, Ord a) => E a -> E a -> E Bool
  EMatch :: E Text -> E Text -> E Bool

instance Show a => Show (E a) where
  show (EBool a)       = show a
  show (EText a)       = show a
  show (EDate a)       = show a
  show (EAmount a)     = show a
  show EVarAmount      = "amount"
  show EVarType        = "type"
  show EVarDescription = "description"
  show EVarDate = "date"
  show EVarImporter    = "importer"
  show (EAbs a)        = "abs(" <> show a <> ")"
  show (EAnd a b)      = "(" <> show a <> " && " <> show b <> ")"
  show (EOr a b)       = "(" <> show a <> " || " <> show b <> ")"
  show (ENot a)        = "!" <> show a
  show (EPlus x y)     = "(" <> show x <> " + " <> show y <> ")"
  show (EMinus x y)    = "(" <> show x <> " - " <> show y <> ")"
  show (ELT a b)       = "(" <> show a <> " < " <> show b <> ")"
  show (ELE a b)       = "(" <> show a <> " <= " <> show b <> ")"
  show (EEQ a b)       = "(" <> show a <> " == " <> show b <> ")"
  show (EGE a b)       = "(" <> show a <> " >= " <> show b <> ")"
  show (EGT a b)       = "(" <> show a <> " > " <> show b <> ")"
  show (ENE a b)       = "(" <> show a <> " <> " <> show b <> ")"
  show (EMatch a b)    = "(" <> show a <> " =~ " <> show b <> ")"

-- Evaluation


data Entry = Entry
  { eDate :: Date
  , eType :: Text
  , eDescription :: Text
  , eAmount      :: Amount
  , eCommodity   :: Commodity
  , eImporter    :: Text
  } deriving (Eq, Show)

type Evaluator = Entry -> Maybe Account

evaluate :: Traversable t => t Rule -> Entry -> Maybe Account
evaluate r = runIdentity . runReaderT (evalRules r)

evalRules :: Traversable t => t Rule -> Reader Entry (Maybe Account)
evalRules rs = msum <$> sequence (evalRule <$> rs)

evalRule :: Rule -> Reader Entry (Maybe Account)
evalRule (Rule e c) = bool Nothing (Just c) <$> evalE e

evalE :: E a -> Reader Entry a
evalE (EBool   a)     = return a
evalE (EText   a)     = return a
evalE (EDate   a)     = return a
evalE (EAmount a)     = return a
evalE EVarAmount      = asks eAmount
evalE EVarDescription = asks eDescription
evalE EVarType        = asks eType
evalE EVarDate        = asks eDate
evalE EVarImporter    = pack . show <$> asks eImporter
evalE (EAbs a    )    = abs <$> evalE a
evalE (EAnd a b  )    = (&&) <$> evalE a <*> evalE b
evalE (EOr  a b  )    = (||) <$> evalE a <*> evalE b
evalE (ENot a    )    = not <$> evalE a
evalE (EPlus  x y)    = (+) <$> evalE x <*> evalE y
evalE (EMinus x y)    = (-) <$> evalE x <*> evalE y
evalE (ELT    a b)    = (<) <$> evalE a <*> evalE b
evalE (ELE    a b)    = (<=) <$> evalE a <*> evalE b
evalE (EEQ    a b)    = (==) <$> evalE a <*> evalE b
evalE (EGE    a b)    = (>=) <$> evalE a <*> evalE b
evalE (EGT    a b)    = (>) <$> evalE a <*> evalE b
evalE (ENE    a b)    = (/=) <$> evalE a <*> evalE b
evalE (EMatch t regex) =
  (=~) <$> (unpack <$> evalE t) <*> (unpack <$> evalE regex)

-- Parser
type Parser = Parsec Void Text

-- The exception exported by this module
newtype ParserException =
  ParserException String
  deriving (Eq)

instance Show ParserException where
  show (ParserException s) = s

instance Exception ParserException

-- parse a file of rules
parseFile :: (MonadIO m, MonadThrow m) => FilePath -> m Rules
parseFile filePath = (liftIO . readFile) filePath >>= parseSource
 where
  parseSource input = either (throwM . ParserException . parseErrorPretty)
                             return
                             (parse rules filePath input)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier =
  cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

accountType :: Parser AccountType
accountType = read . unpack <$> choice
  [ string "Assets"
  , string "Liabilities"
  , string "Expenses"
  , string "Income"
  , string "Equity"
  ]

account :: Parser Account
account =
  lexeme $ Account <$> accountType <* colon <*> (identifier `sepBy` colon)
  where colon = char ':'

symbolOf :: (Functor t, Foldable t) => t (Text, a) -> Parser a
symbolOf = msum . fmap (uncurry (&>))

sym :: a -> Text -> Parser a
sym a b = a <$ symbol b

(<&) :: a -> Text -> Parser a
a <& b = a <$ symbol b

(&>) :: Text -> a -> Parser a
(&>) = flip (<&)

rules :: Parser Rules
rules = between sc eof (many rule)

rule :: Parser Rule
rule = Rule <$> boolExpr <* symbol "->" <*> account <* symbol ";"

amountLiteral :: Parser (E Amount)
amountLiteral = EAmount <$> lexeme amount
  where amount = Sum <$> L.signed sc L.scientific

textLiteral :: Parser (E Text)
textLiteral = EText <$> lexeme quotedText
 where
  quotedText = between q q (takeWhileP (Just "no quote") (/= '"'))
  q          = symbol "\""

dateLiteral :: Parser (E Date)
dateLiteral = EDate <$> lexeme date
 where
  date = fromGreg <$> digits 4 <* dash <*> digits 2 <* dash <*> digits 2
  dash = symbol "-"
  digits n = read <$> count n digitChar

boolLiteral :: Parser (E Bool)
boolLiteral = EBool <$> ("true" &> True <|> "false" &> False)

textExpr :: Parser (E Text)
textExpr = parens textExpr <|> textLiteral <|> "description" &> EVarDescription

textRelation :: Parser (E Text -> E Text -> E Bool)
textRelation = symbolOf [("==", EEQ), ("!=", ENE), ("=~", EMatch)]

dateExpr :: Parser (E Date)
dateExpr = parens dateExpr <|> dateLiteral <|> "date" &> EVarDate

amountExpr :: Parser (E Amount)
amountExpr = makeExprParser amountTerm amountOperators
 where
  amountTerm = parens amountExpr <|> amountLiteral <|> "amount" &> EVarAmount
  amountOperators =
    [[InfixL ("+" &> EPlus), InfixL ("-" &> EMinus), Prefix ("abs" &> EAbs)]]

boolExpr :: Parser (E Bool)
boolExpr = makeExprParser boolTerm boolOperators
 where
  boolTerm =
    parens boolExpr
      <|> boolLiteral
      <|> try dateRelExpr
      <|> amountRelExpr
      <|> textRelExpr
  boolOperators =
    [[Prefix ("not" &> ENot)], [InfixL ("and" &> EAnd), InfixL ("or" &> EOr)]]
  amountRelExpr = comparison amountExpr relation
  textRelExpr   = comparison textExpr textRelation
  dateRelExpr   = comparison dateExpr relation

comparison :: (Applicative f) => f a -> f (a -> a -> b) -> f b
comparison expr rel = do
  e1 <- expr
  r  <- rel
  e2 <- expr
  return $ e1 `r` e2

relation :: (Show a, Ord a) => Parser (E a -> E a -> E Bool)
relation = symbolOf
  [("<=", ELE), (">=", EGE), (">", EGT), ("<", ELT), ("==", EEQ), ("!=", ENE)]

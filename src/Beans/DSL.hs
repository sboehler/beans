{-# LANGUAGE GADTs #-}

module Beans.DSL where

import           Beans.Data.Accounts         (Amount, CommodityName)
import           Beans.Import.CH.Postfinance (Entry (..))
import           Control.Monad               (msum)
import           Control.Monad.Reader        (ReaderT, asks)
import           Data.Functor.Identity       (Identity)
import           Data.Text                   (Text)
import           Data.Time.Calendar          (Day, fromGregorian)

import           Data.Monoid                 (Sum (Sum), (<>))

import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L
import           Text.Megaparsec.Expr

-- This is what the language looks like:
--
-- description = /^Postfinance/ && amount == 40 -> Accounts:Konti:Postfinance
-- True -> Accounts:Expenses:Sonstige

data Rule =
  Rule (E Bool)
       CommodityName

data E a where
  EVarAmount :: E Amount
  EVarDescription :: E Text
  EVarBookingDate :: E Day
  EVarValueDate :: E Day
  EDate :: Day -> E Day
  EAmount :: Amount -> E Amount
  EText :: Text -> E Text
  EBool :: Bool -> E Bool
  EAnd :: E Bool -> E Bool -> E Bool
  EOr :: E Bool -> E Bool -> E Bool
  ENot :: E Bool -> E Bool
  EPlus :: Num a => E a -> E a -> E a
  EMinus :: Num a => E a -> E a -> E a
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
  show EVarDescription = "description"
  show EVarBookingDate = "bookingDate"
  show EVarValueDate   = "valueDate"
  show (EAnd a b)      = show a <> "&&" <> show b
  show (EOr a b)       = show a <> "||" <> show b
  show (ENot a)        = "!" <> show a
  show (EPlus x y)     = show x <> "+" <> show y
  show (EMinus x y)    = show x <> "-" <> show y
  show (ELT a b)       = show a <> "<" <> show b
  show (ELE a b)       = show a <> "<=" <> show b
  show (EEQ a b)       = show a <> "==" <> show b
  show (EGE a b)       = show a <> ">=" <> show b
  show (EGT a b)       = show a <> ">" <> show b
  show (ENE a b)       = show a <> "<>" <> show b
  show (EMatch a b)    = show a <> "=~" <> show b

eval :: E a -> ReaderT Entry Identity a
eval (EBool a)       = return a
eval (EText a)       = return a
eval (EDate a)       = return a
eval (EAmount a)     = return a
eval EVarAmount      = asks _amount
eval EVarDescription = asks _description
eval EVarBookingDate = asks _bookingDate
eval EVarValueDate   = asks _valueDate
eval (EAnd a b)      = (&&) <$> eval a <*> eval b
eval (EOr a b)       = (||) <$> eval a <*> eval b
eval (ENot a)        = not <$> eval a
eval (EPlus x y)     = (+) <$> eval x <*> eval y
eval (EMinus x y)    = (-) <$> eval x <*> eval y
eval (ELT a b)       = (<) <$> eval a <*> eval b
eval (ELE a b)       = (<=) <$> eval a <*> eval b
eval (EEQ a b)       = (==) <$> eval a <*> eval b
eval (EGE a b)       = (>=) <$> eval a <*> eval b
eval (EGT a b)       = (>) <$> eval a <*> eval b
eval (ENE a b)       = (/=) <$> eval a <*> eval b
eval (EMatch _ _)    = return True

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbolOf :: (Functor t, Foldable t) => t (Text, a) -> Parser a
symbolOf = msum . fmap (uncurry (&>))

sym :: a -> Text -> Parser a
sym a b = a <$ symbol b

(<&) :: a -> Text -> Parser a
a <& b = a <$ symbol b

(&>) :: Text -> a -> Parser a
a &> b = b <$ symbol a

exprParser :: Parser (E Bool)
exprParser = between sc eof boolExpr

amountLiteral :: Parser (E Amount)
amountLiteral = EAmount <$> lexeme amount
  where
    amount = Sum <$> L.signed sc L.scientific

textLiteral :: Parser (E Text)
textLiteral = EText <$> lexeme quotedText
  where
    quotedText = between q q (takeWhileP (Just "no quote") (/= '"'))
    q = symbol "\""

dateLiteral :: Parser (E Day)
dateLiteral = EDate <$> lexeme date
  where
    date = fromGregorian <$> digits 4 <* dash <*> digits 2 <* dash <*> digits 2
    dash = symbol "-"
    digits n = read <$> count n digitChar

boolLiteral :: Parser (E Bool)
boolLiteral = EBool <$> ("true" &> True <|> "false" &> False)

textExpr :: Parser (E Text)
textExpr = parens textExpr <|> textLiteral <|> "description" &> EVarDescription

textRelation :: Parser (E Text -> E Text -> E Bool)
textRelation = symbolOf [("==", EEQ), ("!=", ENE), ("=~", EMatch)]

dateExpr :: Parser (E Day)
dateExpr =
  parens dateExpr <|> dateLiteral <|> "valueDate" &> EVarValueDate <|>
  "bookingDate" &> EVarBookingDate

amountExpr :: Parser (E Amount)
amountExpr = makeExprParser amountTerm amountOperators
  where
    amountTerm = parens amountExpr <|> amountLiteral <|> "amount" &> EVarAmount
    amountOperators = [[InfixL ("+" &> EPlus), InfixL ("-" &> EMinus)]]

boolExpr :: Parser (E Bool)
boolExpr = makeExprParser boolTerm boolOperators
  where
    boolTerm =
      parens boolExpr <|> boolLiteral <|> dateRelExpr <|> amountRelExpr <|>
      textRelExpr
    boolOperators =
      [[Prefix ("not" &> ENot)], [InfixL ("and" &> EAnd), InfixL ("or" &> EOr)]]
    amountRelExpr = comparison amountExpr relation
    textRelExpr = comparison textExpr textRelation
    dateRelExpr = comparison dateExpr relation

comparison :: (Applicative f) => f a -> f (a -> a -> b) -> f b
comparison expr rel = flip <$> rel <*> expr <*> expr

relation :: (Show a, Ord a) => Parser (E a -> E a -> E Bool)
relation =
  symbolOf
    [("<=", ELE), (">=", EGE), (">", EGT), ("<", ELT), ("==", EEQ), ("!=", ENE)]

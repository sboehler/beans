module Beans.Megaparsec
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , parseAmount
  , parseAccountType
  , parseDecimal
  , parseISODate
  , parseFormattedDate
  , parseCommodity
  , parseAccount
  , subparse
  , preprocess
  )
where

import           Data.Functor                   ( ($>) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Data.Text                     as T
import           Beans.Model
import           Data.Monoid                    ( Sum(Sum) )
import           Data.Char                      ( isAlphaNum )
import qualified Data.Time.Format              as F

parseAmount :: (MonadParsec e T.Text m) => m () -> m Amount
parseAmount a = Sum <$> parseDecimal a

parseDecimal :: (MonadParsec e T.Text m) => m () -> m Decimal
parseDecimal a = realToFrac <$> signed a scientific

parseCommodity :: (MonadParsec e T.Text m) => m Commodity
parseCommodity =
  Commodity
    <$> (T.cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum)

parseAccountType :: (MonadParsec e T.Text m) => m AccountType
parseAccountType = choice
  [ string "Assets" $> Assets
  , string "Liabilities" $> Liabilities
  , string "Expenses" $> Expenses
  , string "Income" $> Income
  , string "Equity" $> Equity
  ]

parseIdentifier :: (MonadParsec e T.Text m) => m T.Text
parseIdentifier =
  T.cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

parseAccount :: (MonadParsec e T.Text m) => m Account
parseAccount =
  Account <$> parseAccountType <* colon <*> parseIdentifier `sepBy` colon
  where colon = char ':'

parseISODate :: (MonadParsec e T.Text m) => m Date
parseISODate = fromGreg <$> decimal <* dash <*> decimal <* dash <*> decimal
  where dash = char '-'

parseFormattedDate :: (MonadParsec e T.Text m) => String -> m String -> m Date
parseFormattedDate fmt parser = do
  input <- parser
  case parseDate fmt input of
    Just d  -> return d
    Nothing -> fail $ unwords ["Invalid date:", show input]

parseDate :: String -> String -> Maybe Date
parseDate fmt input = Date <$> F.parseTimeM False F.defaultTimeLocale fmt input

subparse :: MonadParsec e s m => m s -> m a -> m a
subparse d p = do
  input <- d
  rest  <- getInput
  setInput input >> p <* setInput rest

preprocess :: MonadParsec e s m => (s -> s) -> m a -> m a
preprocess f p = (f <$> getInput) >>= setInput >> p

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
  )
where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Data.Scientific               as S
import qualified Data.Decimal                  as D
import qualified Data.Text                     as T

import           Beans.Model
import           Data.Monoid                    ( Sum(Sum) )
import           Data.Char                      ( isAlphaNum )
import qualified Data.Time.Format              as F

parseAmount :: (MonadParsec e s m, Token s ~ Char) => m () -> m Amount
parseAmount a = Sum <$> parseDecimal a

parseDecimal :: (MonadParsec e s m, Token s ~ Char) => m () -> m D.Decimal
parseDecimal a = do
  x :: Either Double Integer <- S.floatingOrInteger <$> signed a scientific
  return $ case x of
    Left  f -> D.realFracToDecimal 3 f
    Right i -> D.Decimal 0 i

parseCommodity :: (MonadParsec e T.Text m) => m Commodity
parseCommodity =
  Commodity
    <$> (T.cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum)

parseAccountType :: (MonadParsec e T.Text m) => m AccountType
parseAccountType = read . T.unpack <$> choice
  [ string "Assets"
  , string "Liabilities"
  , string "Expenses"
  , string "Income"
  , string "Equity"
  ]

parseIdentifier :: (MonadParsec e T.Text m) => m T.Text
parseIdentifier =
  T.cons <$> letterChar <*> takeWhileP (Just "alphanumeric") isAlphaNum

parseAccount :: (MonadParsec e T.Text m) => m Account
parseAccount =
  Account <$> parseAccountType <* colon <*> parseIdentifier `sepBy` colon
  where colon = char ':'

parseISODate :: (MonadParsec e T.Text m) => m Date
parseISODate = fromGreg <$> digits 4 <* dash <*> digits 2 <* dash <*> digits 2
 where
  dash = char '-'
  digits n = read <$> count n digitChar

parseFormattedDate :: (MonadParsec e T.Text m) => String -> m String -> m Date
parseFormattedDate fmt parser = do
  inp <- parser
  case parseDate fmt inp of
    Just d  -> return d
    Nothing -> fail $ unwords ["Invalid date:", show inp]

parseDate :: String -> String -> Maybe Date
parseDate fmt input = Date <$> F.parseTimeM False F.defaultTimeLocale fmt input

module Beans.Megaparsec
  ( parseAmount,
    parseAccountType,
    parseValAmount,
    parseISODate,
    parseFormattedDate,
    parseCommodity,
    parseAccount,
    subparse,
    preprocess,
  )
where

import Beans.Account (Account (..), AccountType (..))
import Beans.Amount (Amount)
import Beans.Commodity (Commodity (..))
import qualified Beans.Date as Date
import Beans.ValAmount (ValAmount)
import qualified Beans.ValAmount as ValAmount
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Time.Format as F
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

parseValAmount :: (M.MonadParsec e Text m) => m () -> m ValAmount
parseValAmount sc = ValAmount.new <$> parseAmount sc

parseAmount :: (M.MonadParsec e Text m) => m () -> m Amount
parseAmount sc = realToFrac <$> L.signed sc L.scientific

parseCommodity :: (M.MonadParsec e Text m) => m Commodity
parseCommodity =
  Commodity
    <$> M.takeWhile1P (Just "alphanumeric") Char.isAlphaNum

parseAccountType :: (M.MonadParsec e Text m) => m AccountType
parseAccountType =
  M.choice
    [ M.string "Assets" $> Assets,
      M.string "Liabilities" $> Liabilities,
      M.string "Expenses" $> Expenses,
      M.string "Income" $> Income,
      M.string "Equity" $> Equity,
      M.string "TBD" $> TBD
    ]

parseAccount :: (M.MonadParsec e Text m) => m Account
parseAccount =
  Account <$> parseAccountType <*> M.many parseSegment
  where
    colon = M.char ':'
    parseSegment =
      Text.cons <$> (colon >> M.letterChar) <*> M.takeWhileP (Just "alphanumeric") Char.isAlphaNum

parseISODate :: (M.MonadParsec e Text m) => m Date.Date
parseISODate = do
  y <- L.decimal
  _ <- dash
  m <- L.decimal
  _ <- dash
  d <- L.decimal
  pure $ Date.fromGregorian (y, m, d)
  where
    dash = M.char '-'

parseFormattedDate :: (M.MonadParsec e Text m) => String -> m String -> m Date.Date
parseFormattedDate fmt parser = do
  input <- parser
  case parseDate input of
    Just d -> pure d
    Nothing -> error $ unwords ["Invalid date:", show input]
  where
    parseDate input = Date.Date <$> F.parseTimeM False F.defaultTimeLocale fmt input

subparse :: M.MonadParsec e s m => m s -> m a -> m a
subparse d p = do
  input <- d
  rest <- M.getInput
  M.setInput input >> p <* M.setInput rest

preprocess :: M.MonadParsec e s m => (s -> s) -> m a -> m a
preprocess f p = do
  -- TODO: this changes all the input!
  input' <- f <$> M.getInput
  M.setInput input'
  p

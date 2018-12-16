module Beans.Megaparsec
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , parseAmount
  , parseDecimal
  )
where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import qualified Data.Scientific               as S
import qualified Data.Decimal                  as D

import           Beans.Model
import           Data.Monoid                    ( Sum(Sum) )

parseAmount :: (MonadParsec e s m, Token s ~ Char) => m () -> m Amount
parseAmount a = Sum <$> parseDecimal a

parseDecimal :: (MonadParsec e s m, Token s ~ Char) => m () -> m D.Decimal
parseDecimal a = do
  x :: Either Double Integer <- S.floatingOrInteger <$> signed a scientific
  return $ case x of
    Left  f -> D.realFracToDecimal 3 f
    Right i -> D.Decimal 0 i

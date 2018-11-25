module Beans.Import.CH.SupercardPlus
  ( parse
  , name
  )
where

import           Data.Monoid                    ( Sum(Sum) )
import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , Parser
                                                , askAccount
                                                , parseUtf8
                                                )
import qualified Beans.Data.Map                as M
import           Beans.Model                    ( Amount
                                                , Command(Transaction)
                                                , Commodity(Commodity)
                                                , Date
                                                , Dated(Dated)
                                                , Flag(Complete)
                                                , Position(Position)
                                                , fromGreg
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Group                     ( invert )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Char                      ( isDigit
                                                , isUpper
                                                )
import           Text.Megaparsec                ( (<|>)
                                                , count
                                                , manyTill
                                                , skipManyTill
                                                , many
                                                , manyTill
                                                , takeWhile1P
                                                , takeWhileP
                                                , optional
                                                , eof
                                                , sepEndBy
                                                )
import           Text.Megaparsec.Char           ( anyChar
                                                , string
                                                , char
                                                , upperChar
                                                , digitChar
                                                , eol
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L


name :: T.Text
name = "ch.supercardplus"

parse :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parse = parseUtf8 $ ignoreLine >> command `sepEndBy` eol <* eof

command :: Parser (Dated Command)
command = do
  date              <- dateField <* dateField
  card              <- field $ takeWhile1P (Just "Card Number") isDigit
  sign              <- optional $ string "-"
  commodity         <- Commodity <$> takeWhile1P (Just "Currency") isUpper
  amount            <- amountField
  description       <- quotedField
  location          <- quotedField
  country           <- textField "country"
  zipCode           <- textField "zip code"
  transactionNumber <- quotedField
  transactionType   <- field upperChar <* many digitChar
  account           <- asks cAccount
  let desc = T.unwords
        [ card
        , description
        , location
        , country
        , zipCode
        , transactionNumber
        , T.singleton transactionType
        ]
  otherAccount <- askAccount $ Context
    date
    "expense"
    desc
    (if null sign then amount else invert amount)
    commodity
    name
  let bookings = M.fromListM
        [ (Position account commodity Nothing, M.singleton commodity amount)
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity (-amount)
          )
        ]
  return $ Dated date $ Transaction Complete desc [] bookings

dateField :: Parser Date
dateField = do
  day   <- int 2 <* dot
  month <- int 2 <* dot
  year  <- int 4 <* comma
  return $ fromGreg year month day
 where
  comma = char ','
  dot   = char '.'
  int n = read <$> count n digitChar

amountField :: Parser Amount
amountField = Sum . read . filter (/= '\'') <$> manyTill anyChar (char ',')


quotedField :: Parser Text
quotedField = quote >> T.pack <$> manyTill anyChar (quote >> separator)
  where quote = char '"'

textField :: String -> Parser Text
textField n = field $ takeWhileP (Just n) (/= ',')

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar (eof <|> void eol)

separator :: Parser ()
separator = void (char ',' >> takeWhileP (Just "space") (== ' '))

field :: Parser a -> Parser a
field = L.lexeme separator

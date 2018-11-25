module Beans.Import.CH.SupercardPlus
  ( parse
  , name
  )
where

import           Data.Void                      ( Void )
import           Data.Monoid                    ( Sum(Sum) )
import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , ParserException(ParseError)
                                                , Parser
                                                , askAccount
                                                , parseUtf8
                                                )
import qualified Beans.Data.Map                as M
import           Beans.Model                    ( Amount
                                                , Command(Transaction)
                                                , Commodity(Commodity)
                                                , Date
                                                , parseDate
                                                , Dated(Dated)
                                                , Flag(Complete)
                                                , Position(Position)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Char                      ( isDigit
                                                , isUpper
                                                )
import           Text.Megaparsec                ( (<|>)
                                                , between
                                                , skipManyTill
                                                , many
                                                , customFailure
                                                , takeWhile1P
                                                , takeWhileP
                                                , option
                                                , eof
                                                , Parsec
                                                , sepEndBy
                                                , parseMaybe
                                                )
import           Text.Megaparsec.Char           ( anyChar
                                                , char
                                                , upperChar
                                                , digitChar
                                                , eol
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.List                     as List


name :: T.Text
name = "ch.supercardplus"

parse :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parse = List.sort <$> parseUtf8 (ignoreLine >> command `sepEndBy` eol <* eof)

command :: Parser (Dated Command)
command = do
  date                <- dateField <* dateField
  card                <- field $ takeWhile1P (Just "Card Number") isDigit
  (commodity, amount) <- amountField
  description         <- quotedField
  location            <- quotedField
  country             <- textField "Merchant Cuntry"
  zipCode             <- textField "Merchant Zip Code"
  transactionNumber   <- quotedField
  transactionType     <- field upperChar <* many digitChar
  account             <- asks cAccount
  let desc = T.unwords
        [ card
        , description
        , location
        , country
        , zipCode
        , transactionNumber
        , T.singleton transactionType
        ]
  otherAccount <- askAccount $ Context date "expense" desc amount commodity name
  let bookings = M.fromListM
        [ (Position account commodity Nothing, M.singleton commodity amount)
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity (-amount)
          )
        ]
  return $ Dated date $ Transaction Complete desc [] bookings

dateField :: Parser Date
dateField = do
  inp <- T.unpack <$> textField "Date"
  case parseDate "%-d.%-m.%Y" inp of
    Just d  -> return d
    Nothing -> customFailure $ ParseError $ unwords ["Invalid date:", inp]

amountField :: Parser (Commodity, Amount)
amountField = do
  sign      <- option mempty $ string "-"
  commodity <- Commodity <$> takeWhile1P (Just "Currency") isUpper
  number    <- T.filter (/= '\'') <$> textField "Amount"
  case parseMaybe amountP (sign <> number) of
    Just amount -> return (commodity, amount)
    Nothing     -> customFailure $ ParseError $ unwords
      ["Invalid amount:", T.unpack $ sign <> number]

amountP :: Parsec Void Text Amount
amountP = Sum <$> L.signed (pure ()) L.scientific

quotedField :: Parser Text
quotedField = field
  $ between quote quote (takeWhileP (Just "quoted string") (/= '"'))
  where quote = char '"'

textField :: String -> Parser Text
textField n = field $ takeWhileP (Just n) (/= ',')

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar (eof <|> void eol)

separator :: Parser ()
separator = void (char ',' >> takeWhileP (Just "space") (== ' '))

field :: Parser a -> Parser a
field = L.lexeme separator

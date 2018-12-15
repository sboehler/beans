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
                                                , Command(CmdTransaction)
                                                , Transaction(..)
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
                                                , runReaderT
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
                                                , takeWhile1P
                                                , takeWhileP
                                                , option
                                                , eof
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
  account             <- asks _configAccount
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
        [ (Position account commodity Nothing, M.singleton commodity (-amount))
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity amount
          )
        ]
  return $ Dated date $ CmdTransaction $ Transaction Complete desc [] bookings

dateField :: Parser Date
dateField = do
  inp <- T.unpack <$> textField "Date"
  case parseDate "%-d.%-m.%Y" inp of
    Just d  -> return d
    Nothing -> fail $ unwords ["Invalid date:", show inp]

amountField :: Parser (Commodity, Amount)
amountField = do
  sign      <- option mempty $ string "-"
  commodity <- Commodity <$> takeWhile1P (Just "Currency") isUpper
  number    <- T.filter (/= '\'') <$> textField "Amount"
  p         <- asks $ runReaderT amountP
  case parseMaybe p (sign <> number) of
    Just amount -> return (commodity, amount)
    Nothing     -> fail $ unwords ["Invalid amount:", show $ sign <> number]

amountP :: Parser Amount
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

module Beans.Import.CH.SupercardPlus
  ( parse
  , name
  )
where

import qualified Beans.Data.Map                as M
import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , Parser
                                                , ImporterException
                                                , askAccount
                                                , parseCommands
                                                )
import           Beans.Megaparsec               ( (<|>)
                                                , between
                                                , skipManyTill
                                                , many
                                                , takeWhile1P
                                                , takeWhileP
                                                , option
                                                , eof
                                                , sepEndBy
                                                , parseMaybe
                                                , parseAmount
                                                , parseFormattedDate
                                                , anySingle
                                                , char
                                                , upperChar
                                                , digitChar
                                                , eol
                                                , string
                                                )
import           Beans.Model                    ( Amount
                                                , Command(CmdTransaction)
                                                , Transaction(..)
                                                , Commodity(Commodity)
                                                , Date
                                                , Dated(Dated)
                                                , Flag(Complete)
                                                , Position(Position)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( runReaderT
                                                , asks
                                                )
import qualified Data.ByteString               as B
import           Data.Char                      ( isDigit
                                                , isUpper
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Text.Megaparsec.Char.Lexer    as L


name :: T.Text
name = "ch.supercardplus"

parse :: Config -> B.ByteString -> Either ImporterException [Dated Command]
parse config input = parseCommands
  config
  (ignoreLine >> command `sepEndBy` eol <* eof)
  (decodeUtf8 input)

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
dateField = parseFormattedDate "%-d.%-m.%Y" (T.unpack <$> textField "Date")

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
amountP = parseAmount (pure ())

quotedField :: Parser Text
quotedField = field
  $ between quote quote (takeWhileP (Just "quoted string") (/= '"'))
  where quote = char '"'

textField :: String -> Parser Text
textField n = field $ takeWhileP (Just n) (/= ',')

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anySingle (eof <|> void eol)

separator :: Parser ()
separator = void (char ',' >> takeWhileP (Just "space") (== ' '))

field :: Parser a -> Parser a
field = L.lexeme separator

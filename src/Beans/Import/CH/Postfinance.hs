module Beans.Import.CH.Postfinance
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
import           Beans.Megaparsec               ( count
                                                , try
                                                , parseISODate
                                                , manyTill
                                                , optional
                                                , choice
                                                , takeWhile1P
                                                , skipManyTill
                                                , some
                                                , eof
                                                , parseAmount
                                                , anySingle
                                                , char
                                                , string
                                                , eol
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
import           Control.Monad.Reader           ( asks )
import qualified Data.ByteString               as B
import           Data.Char                      ( isAlphaNum )
import           Data.Group                     ( invert )
import           Data.Text                      ( pack
                                                , Text
                                                )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeLatin1 )
import qualified Text.Megaparsec.Char.Lexer    as L


name :: Text
name = "ch.postfinance" :: Text

parse :: Config -> B.ByteString -> Either ImporterException [Dated Command]
parse config bytes = parseCommands config postfinanceData (decodeLatin1 bytes)

postfinanceData :: Parser [Dated Command]
postfinanceData = do
  commodity <-
    optional (constField ["Datum von:", "Date from:"] *> ignoreField)
    *> constField ["Buchungsart:", "Entry type:"]
    *> ignoreField
    *> constField ["Konto:", "Account:"]
    *> ignoreField
    *> constField ["Währung:", "Currency:"]
    *> currencyField
    <* count 6 ignoreField
  some (command commodity)
    <* ignoreField
    <* constField ["Disclaimer:"]
    <* skipManyTill anySingle eof

command :: Commodity -> Parser (Dated Command)
command commodity = do
  date         <- dateField
  description  <- descriptionField
  amount       <- entryAmount <* count 2 ignoreField
  account      <- asks _configAccount
  otherAccount <- askAccount
    $ Context date "expense" description (invert amount) commodity name
  let bookings = M.fromListM
        [ (Position account commodity Nothing, M.singleton commodity amount)
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity (-amount)
          )
        ]
  return $ Dated
    date
    (CmdTransaction (Transaction Complete description [] bookings))

entryAmount :: Parser Amount
entryAmount = field $ choice [credit, debit]
 where
  debit  = amountField <* separator
  credit = separator *> amountField

currencyField :: Parser Commodity
currencyField = field $ Commodity <$> takeWhile1P (Just "Currency") isAlphaNum

constField :: [Text] -> Parser Text
constField = field . choice . fmap string

dateField :: Parser Date
dateField = try $ field parseISODate

descriptionField :: Parser Text
descriptionField =
  quote
    >> (T.concatMap replace . pack <$> manyTill anySingle
                                                (try (quote >> separator))
       )
 where
  quote = char '"'
  replace '\n' = " "
  replace '\r' = ""
  replace '"'  = "\""
  replace c    = T.singleton c

amountField :: Parser Amount
amountField = parseAmount (pure ())

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anySingle separator

separator :: Parser ()
separator = void $ choice [string ";", eol]

field :: Parser a -> Parser a
field = L.lexeme separator

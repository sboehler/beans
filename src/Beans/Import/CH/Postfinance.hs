module Beans.Import.CH.Postfinance
  ( parse
  , name
  )
where

import qualified Beans.Data.Map                as M
import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , Parser
                                                , askAccount
                                                , parseLatin1
                                                )
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
import           Data.Monoid                    ( Sum(Sum) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( (<|>)
                                                , count
                                                , manyTill
                                                , skipManyTill
                                                , some
                                                )
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , anyChar
                                                , char
                                                , digitChar
                                                , eol
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

name :: Text
name = "ch.postfinance" :: Text

parse :: (MonadIO m, MonadThrow m, MonadReader Config m) => m [Dated Command]
parse = parseLatin1 postfinanceData

postfinanceData :: Parser [Dated Command]
postfinanceData = do
  commodity <- count 3 ignoreLine >> ignoreField >> currency
  commands  <- ignoreLine >> some (command commodity)
  _         <- eol >> ignoreLine >> ignoreLine
  return commands

command :: Commodity -> Parser (Dated Command)
command commodity = do
  date         <- dateField
  description  <- descriptionField
  amount       <- entryAmount <* count 2 ignoreField
  account      <- asks cAccount
  otherAccount <- askAccount
    $ Context date "expense" description (invert amount) commodity name
  let bookings = M.fromListM
        [ (Position account commodity Nothing, M.singleton commodity amount)
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity (-amount)
          )
        ]
  return $ Dated date $ Transaction Complete description [] bookings

entryAmount :: Parser Amount
entryAmount = field $ credit <|> debit
 where
  debit  = amountField <* separator
  credit = separator *> amountField

currency :: Parser Commodity
currency = field $ Commodity . T.pack <$> some alphaNumChar

dateField :: Parser Date
dateField = field (fromGreg <$> int 4 <*> (dash >> int 2) <*> (dash >> int 2))
 where
  dash = char '-'
  int n = read <$> count n digitChar

descriptionField :: Parser Text
descriptionField = quote >> T.pack <$> manyTill anyChar (quote >> separator)
  where quote = char '"'

amountField :: Parser Amount
amountField = Sum <$> L.signed (pure ()) L.scientific

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anyChar eol

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anyChar separator

separator :: Parser ()
separator = void semicolon <|> void eol where semicolon = char ';'

field :: Parser a -> Parser a
field = L.lexeme separator

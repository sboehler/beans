module Beans.Import.CH.Cumulus
  ( parse
  , name
  )
where

import           Beans.Import.Common            ( Config(..)
                                                , Context(..)
                                                , Parser
                                                , ImporterException
                                                , askAccount
                                                , parseCommands
                                                )
import qualified Beans.Data.Map                as M
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
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Beans.Megaparsec               ( (<|>)
                                                , between
                                                , choice
                                                , manyTill
                                                , skipManyTill
                                                , try
                                                , takeWhileP
                                                , parseAmount
                                                , parseFormattedDate
                                                , eof
                                                , many
                                                , anySingle
                                                , eol
                                                , char
                                                , eol
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.ByteString               as B
import           Data.Text.Encoding             ( decodeUtf8 )


name :: T.Text
name = "ch.cumulus"

parse :: Config -> B.ByteString -> Either ImporterException [Dated Command]
parse config input =
  parseCommands config (ignoreLine >> many command <* eof) (decodeUtf8 input)

command :: Parser (Dated Command)
command = do
  date        <- dateField <* ignoreField
  description <- descriptionField
  amount      <- entryAmount
  account     <- asks _configAccount
  let commodity = Commodity "CHF"
  otherAccount <- askAccount
    $ Context date "expense" description amount commodity name
  let bookings = M.fromListM
        [ (Position account commodity Nothing, M.singleton commodity (-amount))
        , ( Position otherAccount commodity Nothing
          , M.singleton commodity amount
          )
        ]
  return $ Dated date $ CmdTransaction $ Transaction Complete
                                                     description
                                                     []
                                                     bookings

dateField :: Parser Date
dateField = parseFormattedDate "%-d.%-m.%Y" (T.unpack <$> quotedField)

entryAmount :: Parser Amount
entryAmount = choice [credit, debit]
 where
  debit  = amountField <* separator
  credit = separator *> amountField

amountField :: Parser Amount
amountField = field $ parseAmount (pure ())

quotedField :: Parser Text
quotedField = field
  $ between quote quote (takeWhileP (Just "quoted string") (/= '"'))
  where quote = char '"'

descriptionField :: Parser Text
descriptionField =
  quote
    >> (T.concatMap replace . T.pack <$> manyTill anySingle
                                                  (try (quote >> separator))
       )
 where
  quote = char '"'
  replace '\n' = " "
  replace '\r' = ""
  replace '"'  = "\""
  replace c    = T.singleton c

ignoreLine :: Parser ()
ignoreLine = void $ skipManyTill anySingle (eof <|> void eol)

ignoreField :: Parser ()
ignoreField = void $ skipManyTill anySingle separator

separator :: Parser ()
separator = void $ choice [string ",", eol]

field :: Parser a -> Parser a
field = L.lexeme separator

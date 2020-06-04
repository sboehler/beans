module Beans.Import.CH.SupercardPlus
  ( parse,
  )
where

import qualified Beans.Account as Account
import Beans.Command (Command (CmdTransaction))
import Beans.Commodity (Commodity (..))
import qualified Beans.Date as Date
import qualified Beans.Import.Common as Common
import qualified Beans.Megaparsec as M
import Beans.Transaction (Posting (..), Transaction (..))
import Beans.ValAmount (ValAmount)
import Control.Monad (void)
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as B
import qualified Data.Char as Char
import Data.Group (invert)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Common.Parser

parse :: Common.Config -> B.ByteString -> Either Common.ImporterException [Command]
parse config input =
  Common.parseCommands
    config
    (ignoreLine >> command `M.sepEndBy` M.eol <* M.eof)
    (Text.decodeUtf8 input)

command :: Parser Command
command = do
  date <- dateField <* dateField
  card <- field $ M.takeWhile1P (Just "Card Number") Char.isDigit
  (commodity, amount) <- amountField
  description <- quotedField
  location <- quotedField
  country <- textField "Merchant Cuntry"
  zipCode <- textField "Merchant Zip Code"
  transactionNumber <- quotedField
  transactionType <- field M.upperChar <* M.many M.digitChar
  account <- Reader.asks Common.account
  let desc =
        Text.unwords
          [ card,
            description,
            location,
            country,
            zipCode,
            transactionNumber,
            Text.singleton transactionType
          ]
  let bookings =
        [ Posting account commodity Nothing (invert amount) Nothing,
          Posting Account.unknown commodity Nothing amount Nothing
        ]
  return $ CmdTransaction $ Transaction date desc [] bookings

dateField :: Parser Date.Date
dateField = M.parseFormattedDate "%-d.%-m.%Y" (Text.unpack <$> textField "Date")

amountField :: Parser (Commodity, ValAmount)
amountField = do
  sign <- M.option mempty $ M.string "-"
  commodity <- Commodity <$> M.takeWhile1P (Just "Currency") Char.isUpper
  number <- Text.filter (/= '\'') <$> textField "Amount"
  p <- Reader.asks $ Reader.runReaderT amountP
  case M.parseMaybe p (sign <> number) of
    Just amount -> return (commodity, amount)
    Nothing -> fail $ unwords ["Invalid amount:", show $ sign <> number]

amountP :: Parser ValAmount
amountP = M.parseValAmount (pure ())

quotedField :: Parser Text
quotedField =
  field $
    M.between quote quote (M.takeWhileP (Just "quoted string") (/= '"'))
  where
    quote = M.char '"'

textField :: String -> Parser Text
textField n = field $ M.takeWhileP (Just n) (/= ',')

ignoreLine :: Parser ()
ignoreLine = void $ M.skipManyTill M.anySingle (M.choice [M.eof, void M.eol])

separator :: Parser ()
separator = void (M.char ',' >> M.takeWhileP (Just "space") (== ' '))

field :: Parser a -> Parser a
field = L.lexeme separator

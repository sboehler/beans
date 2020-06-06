module Beans.Import.CH.Postfinance
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
import Data.Group (invert)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Common.Parser

parse :: Common.Config -> B.ByteString -> Either Common.ImporterException [Command]
parse config bytes = Common.parseCommands config postfinanceData (Text.decodeLatin1 bytes)

postfinanceData :: Parser [Command]
postfinanceData = do
  commodity <-
    M.optional (constField ["Datum von:", "Date from:"] *> ignoreField)
      *> constField ["Buchungsart:", "Entry type:"]
      *> ignoreField
      *> constField ["Konto:", "Account:"]
      *> ignoreField
      *> constField ["Währung:", "Currency:"]
      *> currencyField
      <* M.count 6 ignoreField
  M.some (command commodity)
    <* ignoreField
    <* constField ["Disclaimer:"]
    <* M.skipManyTill M.anySingle M.eof

command :: Commodity -> Parser Command
command commodity = do
  date <- dateField
  description <- descriptionField
  amount <- entryAmount <* M.count 2 ignoreField
  account <- Reader.asks Common.account
  let bookings =
        [ Posting account commodity Nothing amount Nothing,
          Posting Account.unknown commodity Nothing (invert amount) Nothing
        ]
  return $
    CmdTransaction (Transaction date description [] bookings)

entryAmount :: Parser ValAmount
entryAmount = field $ M.choice [credit, debit]
  where
    debit = amountField <* separator
    credit = separator *> amountField

currencyField :: Parser Commodity
currencyField = field M.parseCommodity

constField :: [Text] -> Parser Text
constField = field . M.choice . fmap M.string

dateField :: Parser Date.Date
dateField = field M.parseISODate

descriptionField :: Parser Text
descriptionField =
  quote
    >> ( Text.concatMap replace . Text.pack
           <$> M.manyTill
             M.anySingle
             (M.try (quote >> separator))
       )
  where
    quote = M.char '"'
    replace '\n' = " "
    replace '\r' = ""
    replace '"' = "\""
    replace c = Text.singleton c

amountField :: Parser ValAmount
amountField = M.parseValAmount . pure $ ()

ignoreField :: Parser ()
ignoreField = void $ M.skipManyTill M.anySingle separator

separator :: Parser ()
separator = void $ M.choice [M.string ";", M.eol]

field :: Parser a -> Parser a
field = L.lexeme separator

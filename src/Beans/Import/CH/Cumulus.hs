module Beans.Import.CH.Cumulus
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
parse config input =
  Common.parseCommands config (ignoreLine >> M.many command <* M.eof) (Text.decodeUtf8 input)

command :: Parser Command
command = do
  date <- dateField <* ignoreField
  description <- descriptionField
  amount <- entryAmount
  account <- Reader.asks Common.account
  let commodity = Commodity "CHF"
      bookings =
        [ Posting account commodity Nothing amount Nothing,
          Posting Account.unknown commodity Nothing (invert amount) Nothing
        ]
  return $ CmdTransaction $
    Transaction
      date
      description
      []
      bookings

dateField :: Parser Date.Date
dateField = M.parseFormattedDate "%-d.%-m.%Y" (Text.unpack <$> quotedField)

entryAmount :: Parser ValAmount
entryAmount = M.choice [credit, debit]
  where
    debit = amountField <* separator
    credit = invert <$> (separator *> amountField)

amountField :: Parser ValAmount
amountField = field $ M.parseValAmount (pure ())

quotedField :: Parser Text
quotedField =
  field $
    M.between quote quote (M.takeWhileP (Just "quoted string") (/= '"'))
  where
    quote = M.char '"'

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

ignoreLine :: Parser ()
ignoreLine = void $ M.skipManyTill M.anySingle (M.choice [M.eof, void M.eol])

ignoreField :: Parser ()
ignoreField = void $ M.skipManyTill M.anySingle separator

separator :: Parser ()
separator = void $ M.choice [M.string ",", M.eol]

field :: Parser a -> Parser a
field = L.lexeme separator

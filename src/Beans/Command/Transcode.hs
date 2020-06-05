module Beans.Command.Transcode
  ( run,
    Options (..),
  )
where

import qualified Beans.Amount as Amount
import qualified Beans.Balance as Balance
import Beans.Commodity (Commodity)
import qualified Beans.Ledger as Ledger
import Beans.Ledger (Ledger (..))
import Beans.LedgerStep (LedgerStep (..))
import Beans.Parser (parseFile)
import qualified Beans.Process as Process
import Beans.Transaction (Posting (..), Transaction (..))
import Beans.ValAmount (ValAmount (..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Data.Map.Strict.Extended as Map
import Data.Text (Text)
import Data.Text.IO as TextIO
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc ((<+>), Doc, pretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as P

data Options
  = Options
      { trnCommodity :: Commodity,
        trnSourceFile :: FilePath,
        trnTargetFile :: FilePath
      }
  deriving (Show)

run :: (MonadThrow m, MonadReader Options m, MonadIO m) => m ()
run = do
  Options commodity source target <- Reader.ask
  d <- parseFile source
  let l = Ledger.fromDirectives d
  minDate <- Ledger.minDate l
  (l', _) <- Process.runLedger (Balance.new minDate [commodity]) l
  let text = transcode commodity l'
  liftIO $ TextIO.writeFile target text

transcode :: Commodity -> Ledger -> Text
transcode c (Ledger l) = render $ P.vsep $ fmap (<+> P.hardline) (options : docs)
  where
    options = P.hsep [pretty $ "option \"operating_currency\" \"" <> show c <> "\""]
    docs = concatMap ppl l

ppl :: LedgerStep -> [Doc a]
ppl (LS _ open _ transactions _ closings) =
  (pretty <$> open) ++ (ppt <$> transactions) ++ (pretty <$> closings)

ppt :: Transaction -> Doc a
ppt (Transaction d desc tags (p : ps)) = P.hang 2 (P.vsep [header, postings])
  where
    header = P.hsep ([pretty d, pretty '*', pretty $ "\"" <> desc <> "\""] ++ (pretty <$> tags))
    postings = P.vsep (ppw p : (ppp <$> ps))
ppt _ = error "Empty transaction"

ppp :: Posting -> Doc a
ppp (Posting a _ _ (ValAmount _ values) _) =
  let (c, v) = Map.elemAt 0 values
   in P.hsep [pretty a, pretty $ Amount.showFixed v, pretty c]

ppw :: Posting -> Doc a
ppw (Posting a _ _ _ _) =
  P.hsep [pretty a]

render :: Doc a -> Text
render = P.renderStrict . P.layoutPretty P.defaultLayoutOptions

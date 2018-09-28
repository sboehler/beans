module Beans.Import
  ( importCommand
  ) where

import           Beans.Data.Accounts         (Account)
import           Beans.Data.Directives       (Command (..), DatedCommand (..),
                                              Flag (Complete), Transaction (..))
import qualified Beans.Data.Map              as M
import qualified Beans.Import.CH.Postfinance
import           Beans.Import.Common         (Entry (..))
import           Beans.Import.DSL            (Rule, evaluate, parseFile)
import           Beans.Options               (ImportOptions (..), Importer (..))
import           Beans.Pretty                ()
import           Control.Applicative         (ZipList (ZipList), getZipList)
import           Control.Exception           (Exception)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ask, asks)
import qualified Data.Text.Prettyprint.Doc   as P

data ImportException = NoAccountFound [Rule] Entry deriving (Show)

instance Exception ImportException

importCommand :: (MonadReader ImportOptions m, MonadThrow m, MonadIO m) => m ()
importCommand = do
  parse                        <- getParser
  entries                      <- asks optData >>= parse
  rules                        <- asks optConfig >>= parseFile
  ImportOptions { optAccount } <- ask
  accounts                     <- mapM (eval rules) entries
  let transactions =
        getZipList
          $   mkTransaction
          <$> ZipList entries
          <*> pure optAccount
          <*> ZipList accounts
  liftIO $ print $ P.sep (P.pretty <$> transactions)

eval :: MonadThrow m => [Rule] -> Entry -> m Account
eval rules entry =
  maybe (throwM $ NoAccountFound rules entry) return (evaluate rules entry)

mkTransaction :: Entry -> Account -> Account -> DatedCommand
mkTransaction Entry {..} account otherAccount =
  DatedCommand eBookingDate $ TransactionCommand $ Transaction Complete
                                                               eDescription
                                                               []
                                                               postings
 where
  postings = M.fromListM
    [ ((account, eCommodity, Nothing)     , M.singleton eCommodity eAmount)
    , ((otherAccount, eCommodity, Nothing), M.singleton eCommodity (-eAmount))
    ]


getParser
  :: (MonadThrow m, MonadIO m, MonadReader ImportOptions m)
  => m (FilePath -> m [Entry])
getParser = select <$> asks optImporter
  where select Postfinance = Beans.Import.CH.Postfinance.readCSV

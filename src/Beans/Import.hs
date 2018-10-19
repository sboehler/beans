module Beans.Import
  ( importCommand
  )
where

import           Beans.Data.Accounts                      ( Account
                                                          , Date(Date)
                                                          , Position(..)
                                                          )
import           Beans.Data.Directives                    ( Command(..)
                                                          , Dated(..)
                                                          , Flag(Complete)
                                                          )
import qualified Beans.Data.Map                as M
import qualified Beans.Import.CH.Postfinance
import           Beans.Import.Common                      ( Entry(..) )
import           Beans.Import.DSL                         ( Rule
                                                          , evaluate
                                                          , parseFile
                                                          )
import           Beans.Options                            ( ImportOptions(..) )
import           Beans.Pretty                             ( )
import           Control.Applicative                      ( ZipList(ZipList)
                                                          , getZipList
                                                          )
import           Data.Text                                ( Text )
import           Control.Exception                        ( Exception )
import           Control.Monad.Catch                      ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Monoid                              ( (<>) )
import qualified Data.Text.Prettyprint.Doc     as P

data ImportException = NoAccountFound [Rule] Entry | InvalidImporter String deriving (Show)

instance Exception ImportException

importCommand :: (MonadThrow m, MonadIO m) => ImportOptions -> m ()
importCommand ImportOptions {..} = do
  parse    <- getParser impOptImporter
  entries  <- parse impOptData
  rules    <- parseFile impOptConfig
  accounts <- mapM (eval rules) entries
  let transactions =
        getZipList
          $   mkTransaction
          <$> ZipList entries
          <*> pure impOptAccount
          <*> ZipList accounts
  liftIO $ print $ P.sep (P.pretty <$> transactions)

eval :: MonadThrow m => [Rule] -> Entry -> m Account
eval rules entry =
  maybe (throwM $ NoAccountFound rules entry) return (evaluate rules entry)

mkTransaction :: Entry -> Account -> Account -> Dated Command
mkTransaction Entry {..} account otherAccount =
  Dated (Date eBookingDate) $ Transaction Complete eDescription [] postings
 where
  postings = M.fromListM
    [ (Position account eCommodity Nothing, M.singleton eCommodity eAmount)
    , ( Position otherAccount eCommodity Nothing
      , M.singleton eCommodity (-eAmount)
      )
    ]


getParser :: (MonadIO m, MonadThrow m) => Text -> m (FilePath -> m [Entry])
getParser n
  | n == Beans.Import.CH.Postfinance.name = return
    Beans.Import.CH.Postfinance.parseEntries
  | otherwise = throwM $ InvalidImporter $ "Invalid importer: " <> show n

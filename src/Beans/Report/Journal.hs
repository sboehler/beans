module Beans.Report.Journal
  ( createJournal
  , Journal(..)
  )
where

import           Prelude                 hiding ( filter )
import qualified Data.Text                     as T
import           Text.Regex.PCRE                ( (=~) )

import           Beans.Options                  ( JournalOptions(..) )
import           Beans.Model                    ( Date
                                                , Command(..)
                                                , Filter(..)
                                                , filter
                                                , Dated(..)
                                                , Amount
                                                , Ledger
                                                , Accounts
                                                , Commodity
                                                , Position(..)
                                                , Account
                                                , format
                                                )
import qualified Beans.Model                   as BM
import qualified Data.List                     as List
import           Beans.Accounts                 ( sumUntil )
import qualified Beans.Data.Map                as M
import           Data.Text                      ( Text )
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Maybe                     ( mapMaybe )
import qualified Beans.Table                   as TB
import           Control.Lens

data Journal = Journal {
  rHeader :: Dated Item,
  rItems :: M.Map Date [Item],
  rFooter :: Dated Item
  } deriving (Show)

data Item = Item {
  eDescription :: Text,
  ePostings :: [(Commodity, Amount)],
  eOtherPostings :: [((Account, Commodity), Amount)]
  } deriving (Show)

instance TB.Table Journal where
  toTable (Journal (Dated t0 header) items (Dated t1 footer)) = concat
    [ sep
    , itemToTable (formatDate t0) header
    , sep
    , (concatMap datedItemToTable . M.toList) items
    , sep
    , itemToTable (formatDate t1) footer
    , sep
    ]
   where
    sep        = [replicate 7 TB.Separator]
    formatDate = TB.AlignLeft . T.pack . show

createJournal :: MonadThrow m => JournalOptions -> Ledger -> m Journal
createJournal JournalOptions {..} ledger = do
  (accounts0, l') <- sumUntil jrnOptFrom filtered' mempty
  (accounts1, _ ) <- sumUntil jrnOptTo l' accounts0
  return $ Journal
    { rHeader = accountsToItem jrnOptRegex jrnOptFrom accounts0
    , rItems  = items
    , rFooter = accountsToItem jrnOptRegex jrnOptTo accounts1
    }
 where
  filtered' = filter (PeriodFilter jrnOptFrom jrnOptTo)
    $ filter (Filter $ T.unpack jrnOptRegex) ledger
  items = toItem' jrnOptRegex <$> filtered'

accountsToItem :: Text -> Date -> Accounts -> Dated Item
accountsToItem regex date accounts =
  let filteredAccounts =
        List.filter ((=~ T.unpack regex) . show . _positionAccount . fst)
          $ M.toList accounts
      amounts = M.toList $ mconcat (snd <$> filteredAccounts)
  in  Dated
        date
        (Item {eDescription = regex, ePostings = amounts, eOtherPostings = []})

toItem' :: Text -> [Command] -> [Item]
toItem' regex = mapMaybe (toItem regex)

toItem :: Text -> Command -> Maybe Item
toItem regex (CmdTransaction t) = Just $ Item
  { eDescription   = t ^. BM.description
  , ePostings      = concat $ M.toList . snd <$> postings
  , eOtherPostings = concat $ convert <$> otherPostings
  }
 where
  allPostings = M.toList (t ^. BM.postings)
  matchRegex = view $ _1 . BM.account . to ((=~ T.unpack regex) . show)
  (postings, otherPostings) = List.partition matchRegex allPostings
  convert (p, amounts) =
    (\(c, a) -> ((p ^. BM.account, c), a)) <$> M.toList amounts
toItem _ _ = Nothing

datedItemToTable :: (Date, [Item]) -> [[TB.Cell]]
datedItemToTable (d, items) =
  concat
      (zipWith itemToTable
               ((TB.AlignLeft . T.pack . show) d : repeat TB.Empty)
               items
      )
    ++ [replicate 7 TB.Empty]

itemToTable :: TB.Cell -> Item -> [[TB.Cell]]
itemToTable date Item {..} = List.transpose $ align
  [ [date]
  , amounts
  , commodities
  , descriptions
  , otherAccounts
  , otherAmounts
  , otherCommodities
  ]
 where
  descriptions  = TB.AlignLeft <$> T.chunksOf 60 eDescription
  amounts       = TB.AlignRight . format . snd <$> ePostings
  commodities   = TB.AlignLeft . T.pack . show . fst <$> ePostings
  otherAccounts = TB.AlignLeft . T.pack . show . fst . fst <$> eOtherPostings
  otherAmounts  = TB.AlignRight . format . snd <$> eOtherPostings
  otherCommodities =
    TB.AlignLeft . T.pack . show . snd . fst <$> eOtherPostings

align :: [[TB.Cell]] -> [[TB.Cell]]
align columns = do
  col <- columns
  return $ take l $ col ++ repeat TB.Empty
  where l = maximum $ length <$> columns

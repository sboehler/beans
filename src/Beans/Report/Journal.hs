module Beans.Report.Journal
  ( createJournal
  , Journal(..)
  , journalToTable
  )
where

import           Prelude                 hiding ( filter )
import qualified Data.Text                     as T
import           Text.Regex.PCRE                ( (=~) )

import           Beans.Options                  ( JournalOptions(..) )
import           Beans.Model                    ( Date
                                                , Transaction(..)
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

import qualified Data.List                     as List
import           Beans.Accounts                 ( sumUntil )
import qualified Beans.Data.Map                as M
import           Data.Text                      ( Text )
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Maybe                     ( catMaybes )
import           Beans.Table                    ( Cell(..)
                                                , Table(..)
                                                )

data Journal = Journal {
  rHeader :: Dated Item,
  rItems :: M.Map Date [Item],
  rFooter :: Dated Item
  } deriving (Show)

instance Table Journal where
  toTable = journalToTable

data Item = Item {
  eDescription :: Text,
  eAccoun_transactionPostings :: [(Commodity, Amount)],
  eOtherPostings :: [(Account, Commodity, Amount)]
  } deriving (Show)

createJournal :: MonadThrow m => JournalOptions -> Ledger -> m Journal
createJournal JournalOptions {..} ledger = do
  let filtered = filter (Filter (T.unpack jrnOptRegex)) ledger
      items =
        toItem' jrnOptRegex
          <$> filter (PeriodFilter jrnOptFrom jrnOptTo) filtered
  (accounts0, l') <- sumUntil jrnOptFrom filtered mempty
  (accounts1, _ ) <- sumUntil jrnOptTo l' accounts0
  return $ Journal
    { rHeader = accountsToItem jrnOptRegex jrnOptFrom accounts0
    , rItems  = items
    , rFooter = accountsToItem jrnOptRegex jrnOptTo accounts1
    }

accountsToItem :: Text -> Date -> Accounts -> Dated Item
accountsToItem regex date accounts =
  let filteredAccounts =
        List.filter ((=~ T.unpack regex) . show . _positionAccount . fst)
          $ M.toList accounts
      amounts = M.toList $ mconcat (snd <$> filteredAccounts)
  in  Dated
        date
        (Item
          { eDescription                = regex
          , eAccoun_transactionPostings = amounts
          , eOtherPostings              = []
          }
        )

toItem' :: Text -> [Command] -> [Item]
toItem' regex cmds = catMaybes $ fmap (toItem regex) cmds

toItem :: Text -> Command -> Maybe Item
toItem regex (CmdTransaction Transaction { _transactionDescription, _transactionPostings })
  = let (accoun_transactionPostings, otherPostings) =
          List.partition ((=~ T.unpack regex) . show . _positionAccount . fst)
            $ M.toList _transactionPostings
    in  Just $ Item
          { eDescription = _transactionDescription
          , eAccoun_transactionPostings = concat
            $   toAccoun_transactionPostings
            <$> accoun_transactionPostings
          , eOtherPostings = concat $ toOtherPostings <$> otherPostings
          }
 where
  toAccoun_transactionPostings (_, amounts) = M.toList amounts
  toOtherPostings (Position {..}, amounts) =
    f _positionAccount <$> M.toList amounts
  f a (commodity, amount) = (a, commodity, amount)
toItem _ _ = Nothing


-- Formatting a report as a table
journalToTable :: Journal -> [[Cell]]
journalToTable (Journal (Dated t0 header) items (Dated t1 footer)) = concat
  [ [replicate 7 Separator]
  , itemToTable ((AlignLeft . T.pack . show) t0) header
  , [replicate 7 Separator]
  , (concatMap datedItemToTable . M.toList) items
  , [replicate 7 Separator]
  , itemToTable ((AlignLeft . T.pack . show) t1) footer
  , [replicate 7 Separator]
  ]

datedItemToTable :: (Date, [Item]) -> [[Cell]]
datedItemToTable (d, items) =
  concat
      (zipWith itemToTable ((AlignLeft . T.pack . show) d : repeat Empty) items)
    ++ [replicate 7 Empty]

itemToTable :: Cell -> Item -> [[Cell]]
itemToTable date Item {..}
  = let
      dates    = take nbrRows $ date : repeat Empty
      desc     = T.chunksOf 40 eDescription
      quantify = take nbrRows . (++ repeat "")
      amounts =
        AlignRight <$> quantify (format . snd <$> eAccoun_transactionPostings)
      commodities = AlignLeft
        <$> quantify (T.pack . show . fst <$> eAccoun_transactionPostings)
      descriptions  = AlignLeft <$> quantify desc
      otherAccounts = AlignLeft <$> quantify
        (T.pack . show . (\(account, _, _) -> account) <$> eOtherPostings)
      otherAmounts = AlignRight
        <$> quantify (format . (\(_, _, amount) -> amount) <$> eOtherPostings)
      otherCommodities =
        AlignLeft
          <$> quantify
                (   T.pack
                .   show
                .   (\(_, commodity, _) -> commodity)
                <$> eOtherPostings
                )
      nbrRows =
        maximum [1, length eAccoun_transactionPostings, length eOtherPostings]
    in
      List.transpose
        [ dates
        , amounts
        , commodities
        , descriptions
        , otherAccounts
        , otherAmounts
        , otherCommodities
        ]

module Beans.Report.Journal
  ( createReport
  , Report(..)
  , reportToTable
  )
where

import qualified Data.Text                     as T
import           Text.Regex.PCRE                          ( (=~) )
import           Beans.Data.Directives                    ( Command(..)
                                                          , Dated(..)
                                                          )
import           Beans.Options                            ( JournalOptions(..)
                                                          , Filter(..)
                                                          )
import           Beans.Ledger                             ( Ledger )
import           Beans.Data.Accounts                      ( Date(..)
                                                          , Amount
                                                          , Accounts
                                                          , Commodity
                                                          , Position(..)
                                                          , Account
                                                          )
import qualified Data.List                     as List
import           Beans.Accounts                           ( calculateAccountsForDays
                                                          )
import qualified Beans.Ledger                  as L
import qualified Beans.Data.Map                as M
import           Data.Text                                ( Text )
import           Control.Monad.Catch                      ( MonadThrow )
import           Data.Maybe                               ( mapMaybe )
import           Beans.Table                              ( Cell(..)
                                                          , formatStandard
                                                          )

createReport :: MonadThrow m => JournalOptions -> Ledger -> m Report
createReport JournalOptions {..} ledger = do
  let filtered     = L.filter jrnOptFilter ledger
      transactions = L.filter (PeriodFilter jrnOptFrom jrnOptTo) filtered
      items        = mapMaybe (toItem jrnOptFilter) transactions
  [a0, a1] <- calculateAccountsForDays filtered [jrnOptFrom, jrnOptTo] mempty
  return $ Report
    { rHeader = accountsToItem jrnOptFilter jrnOptFrom a0
    , rItems  = M.fromListM items
    , rFooter = accountsToItem jrnOptFilter jrnOptTo a1
    }

data Report = Report {
  rHeader :: Dated Item,
  rItems :: M.Map Date [Item],
  rFooter :: Dated Item
  } deriving (Show)

data Item = Item {
  eDescription :: Text,
  eAccountPostings :: [(Commodity, Amount)],
  eOtherPostings :: [(Account, Commodity, Amount)]
  } deriving (Show)

accountsToItem :: Filter -> Date -> Accounts -> Dated Item
accountsToItem (Filter regex) date accounts =
  let filteredAccounts =
        List.filter ((=~ regex) . show . pAccount . fst) $ M.toList accounts
      amounts = M.toList $ mconcat (snd <$> filteredAccounts)
  in  Dated
        date
        (Item
          { eDescription     = T.pack regex
          , eAccountPostings = amounts
          , eOtherPostings   = []
          }
        )
accountsToItem _ _ _ = error "Fix this"

toItem :: Filter -> Dated Command -> Maybe (Date, [Item])
toItem (Filter regex) (Dated d Transaction {..})
  = let (accountPostings, otherPostings) =
          List.partition ((=~ regex) . show . pAccount . fst)
            $ M.toList tPostings
    in  Just
          ( d
          , [ Item
                { eDescription     = tDescription
                , eAccountPostings = concat
                  $   toAccountPostings
                  <$> accountPostings
                , eOtherPostings   = concat $ toOtherPostings <$> otherPostings
                }
            ]
          )
 where
  toAccountPostings (_, amounts) = M.toList amounts
  toOtherPostings (Position {..}, amounts) = f pAccount <$> M.toList amounts
  f a (commodity, amount) = (a, commodity, amount)
toItem _ _ = Nothing


-- Formatting a report into rows
reportToTable :: Report -> [[Cell]]
reportToTable (Report (Dated t0 header) items (Dated t1 footer)) = concat
  [ [replicate 7 Separator]
  , itemToRows ((AlignLeft . T.pack . show) t0) header
  , [replicate 7 Separator]
  , (concatMap datedItemToRows . M.toList) items
  , [replicate 7 Separator]
  , itemToRows ((AlignLeft . T.pack . show) t1) footer
  , [replicate 7 Separator]
  ]

datedItemToRows :: (Date, [Item]) -> [[Cell]]
datedItemToRows (d, items) =
  concat
      (zipWith itemToRows ((AlignLeft . T.pack . show) d : repeat Empty) items)
    ++ [replicate 7 Empty]

itemToRows :: Cell -> Item -> [[Cell]]
itemToRows date Item {..} =
  let
    dates    = take nbrRows $ date : repeat Empty
    desc     = T.chunksOf 40 eDescription
    quantify = take nbrRows . (++ repeat "")
    amounts =
      AlignRight <$> quantify (formatStandard . snd <$> eAccountPostings)
    commodities =
      AlignLeft <$> quantify (T.pack . show . fst <$> eAccountPostings)
    descriptions  = AlignLeft <$> quantify desc
    otherAccounts = AlignLeft <$> quantify
      (T.pack . show . (\(account, _, _) -> account) <$> eOtherPostings)
    otherAmounts = AlignRight <$> quantify
      (formatStandard . (\(_, _, amount) -> amount) <$> eOtherPostings)
    otherCommodities = AlignLeft <$> quantify
      (T.pack . show . (\(_, commodity, _) -> commodity) <$> eOtherPostings)
    nbrRows = maximum [1, length eAccountPostings, length eOtherPostings]--, length desc]
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

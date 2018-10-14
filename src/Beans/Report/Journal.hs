module Beans.Report.Journal
  ( createReport
  , Report(..)
  )
where

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


createReport :: MonadThrow m => JournalOptions -> Ledger -> m Report
createReport JournalOptions {..} ledger = do
  let filtered     = L.filter jrnOptFilter ledger
      transactions = L.filter (PeriodFilter jrnOptFrom jrnOptTo) filtered
      items        = mapMaybe (toItem jrnOptFilter) transactions
  [a0, a1] <- calculateAccountsForDays filtered [jrnOptFrom, jrnOptTo] mempty
  return $ Report
    { rHeader = accountsToItem jrnOptFilter jrnOptFrom a0
    , rItems  = items
    , rFooter = accountsToItem jrnOptFilter jrnOptTo a1
    }

data Report = Report {
  rHeader :: Item,
  rItems :: [Item],
  rFooter :: Item
  } deriving (Show)

data Item = Item {
  iDate :: Date,
  eDescription :: Text,
  eAccountPostings :: [(Commodity, Amount)],
  eOtherPostings :: [(Account, Commodity, Amount)]
  } deriving (Show)

accountsToItem :: Filter -> Date -> Accounts -> Item
accountsToItem (Filter regex) date accounts =
  let postings =
        List.filter ((=~ regex) . show . pAccount . fst) $ M.toList accounts
      amounts = concat $ M.toList . snd <$> postings
  in  Item
        { iDate            = date
        , eDescription     = "Total"
        , eAccountPostings = amounts
        , eOtherPostings   = []
        }
accountsToItem _ _ _ = error "Fix this"


toItem :: Filter -> Dated Command -> Maybe Item
toItem (Filter regex) (Dated d Transaction {..}) =
  let (accountPostings, otherPostings) =
        List.partition ((=~ regex) . show . pAccount . fst) $ M.toList tPostings
  in  Just $ Item
        { iDate            = d
        , eDescription     = tDescription
        , eAccountPostings = concat $ toAccountPostings <$> accountPostings
        , eOtherPostings   = concat $ toOtherPostings <$> otherPostings
        }
 where
  toAccountPostings (_, amounts) = M.toList amounts
  toOtherPostings (Position {..}, amounts) = f pAccount <$> M.toList amounts
  f a (commodity, amount) = (a, commodity, amount)
toItem _ _ = Nothing

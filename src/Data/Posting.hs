module Data.Posting
  ( PostingPrice(..)
  , Posting(..)
  ) where

import Control.Lens (makePrisms)
import Data.Account (AccountName)
import Data.Amount (Amount)
import Data.Date (Date)
import Data.Decimal (Decimal)
import Data.Price (Price)
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc
       (Pretty, (<+>), encloseSep, pretty)

data Posting = Posting
  { _postingAccountName :: AccountName
  , _amount :: Amount Decimal
  , _price :: Maybe PostingPrice
  , _lotCost :: Maybe (Price Decimal)
  , _lotLabel :: Maybe Text
  , _lotDate :: Maybe Date
  } deriving (Show, Eq)

instance Pretty Posting where
  pretty Posting {..} =
    pretty _postingAccountName <+>
    pretty _amount <+> pretty _price <+> pretty' _lotCost _lotLabel _lotDate
    where
      pretty' Nothing Nothing Nothing = mempty
      pretty' c' l' d' =
        encloseSep "{" "}" "," [pretty c', pretty l', pretty d']

data PostingPrice
  = UnitPrice { _unitPrice :: Price Decimal }
  | TotalPrice { _totalPrice :: Amount Decimal }
  deriving (Show, Eq)

instance Pretty PostingPrice where
  pretty (UnitPrice p) = "@" <+> pretty p
  pretty (TotalPrice a) = "@@" <+> pretty a

makePrisms ''PostingPrice

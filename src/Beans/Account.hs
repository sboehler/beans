module Beans.Account
  ( Account (Account),
    AccountType (..),
    shorten,
    match,
    split,
    valuationAccount,
    unknown,
  )
where

import Beans.Filter (AccountFilter (AccountFilter))
import Data.Hashable (Hashable (..))
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Text.Regex.PCRE ((=~))

data AccountType = Assets | Liabilities | Equity | Income | Expenses | TBD
  deriving (Eq, Ord, Read, Show)

data Account
  = Account AccountType [Text]
  deriving (Eq, Ord)

instance Pretty Account where
  pretty = pretty . show

instance Show Account where
  show (Account t n) = List.intercalate ":" (show t : (Text.unpack <$> n))

instance Hashable Account where
  hashWithSalt n a = hashWithSalt n (show a)
  hash a = hash (show a)

shorten :: Int -> Account -> Account
shorten d (Account t s) = Account t (take d s)

match :: AccountFilter -> Account -> Bool
match (AccountFilter regex) = (=~ regex) . show

split :: Account -> [Text]
split (Account t s) = (Text.pack . show $ t) : s

valuationAccount :: Account
valuationAccount = Account Equity ["Equity"]

unknown :: Account
unknown = Account TBD []

module Data.Transaction
  ( Transaction(..)
  , Flag(..)
  , Tag(..)
  ) where

import Control.Lens (makeLenses)
import Data.Date (Date)
import Data.Posting (Posting)
import Data.Text.Lazy (Text)
import Data.Text.Prettyprint.Doc
       (Pretty, (<+>), (<>), cat, dquotes, indent, line, pretty, vcat)

data Transaction = Transaction
  { _date :: Date
  , _flag :: Flag
  , _description :: Text
  , _tags :: [Tag]
  , _postings :: [Posting]
  } deriving (Eq, Show)

instance Pretty Transaction where
  pretty Transaction {..} =
    pretty _date <+>
    pretty _flag <+>
    dquotes (pretty _description) <+>
    cat (map pretty _tags) <> line <> (indent 2 . vcat) (map pretty _postings)

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Show)

instance Pretty Flag where
  pretty Complete = "*"
  pretty Incomplete = "!"

newtype Tag =
  Tag Text
  deriving (Show, Eq)

instance Pretty Tag where
  pretty (Tag t) = pretty t

makeLenses ''Tag

module Beans.Option (Option (Option)) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc ((<+>), Pretty (pretty))
import qualified Text.Megaparsec.Pos as P

data Option = Option P.SourcePos Text Text
  deriving (Eq, Ord, Show)

instance Pretty Option where
  pretty (Option _ d t) = "option" <+> pretty d <+> pretty t

module Beans.Include (Include (Include)) where

import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))
import qualified Text.Megaparsec.Pos as P

data Include = Include
  { position :: P.SourcePos,
    filePath :: FilePath
  }
  deriving (Eq, Ord, Show)

instance Pretty Include where
  pretty (Include _ filePath) = "include" <+> pretty filePath

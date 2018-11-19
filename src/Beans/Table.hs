module Beans.Table
  ( Cell(..)
  , Table(..)
  , showTable
  )
where

import           Data.List                                ( transpose
                                                          , foldl'
                                                          )
import           Data.Monoid                              ( (<>) )
import           Prelude                           hiding ( lines )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T


class Table a where
  toTable :: a -> [[Cell]]

data Cell
  = AlignLeft Text
  | AlignRight Text
  | AlignCenter Text
  | Separator
  | IndentBy Int
             Text
  | Empty

showTable :: Table a => a -> Text
showTable t =
  let rows         = toTable t
      columnWidths = [ maximum $ width <$> column | column <- transpose rows ]
      l            = pad "|" . foldl' combine "" . zip columnWidths <$> rows
  in  T.unlines l

width :: Cell -> Int
width Separator       = 0
width Empty           = 0
width (AlignLeft   t) = T.length t
width (AlignRight  t) = T.length t
width (AlignCenter t) = T.length t
width (IndentBy n t ) = n + T.length t

pad :: Text -> Text -> Text
pad padding content = padding <> content <> padding

combine :: Text -> (Int, Cell) -> Text
combine "" b                = format b
combine t  b@(_, Separator) = t <> "+" <> format b
combine t  b                = t <> "|" <> format b

format :: (Int, Cell) -> Text
format (n, Separator    ) = pad "-" $ T.replicate n "-"
format (n, AlignLeft t  ) = pad " " $ T.justifyLeft n ' ' t
format (n, AlignRight t ) = pad " " $ T.justifyRight n ' ' t
format (n, AlignCenter t) = pad " " $ T.center n ' ' t
format (n, Empty        ) = pad " " $ T.replicate n " "
format (n, IndentBy i t) =
  pad " " (T.replicate i " " <> T.justifyLeft (n - i) ' ' t)

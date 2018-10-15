module Beans.Table
  ( Cell(..)
  , showTable
  , formatStandard
  )
where

import           Data.List                                ( transpose )
import           Data.Monoid                              ( Sum
                                                          , getSum
                                                          , (<>)
                                                          )
import           Data.Scientific                          ( FPFormat(Fixed)
                                                          , Scientific
                                                          , formatScientific
                                                          )
import           Prelude                           hiding ( lines )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T


data Cell
  = AlignLeft Text
  | AlignRight Text
  | AlignCenter Text
  | Separator
  | IndentBy Int
             Text
  | Empty


showTable :: [[Cell]] -> Text
showTable rows =
  let columnWidths = [ maximum $ width <$> column | column <- transpose rows ]
      separators =
        [ T.replicate columnWidth "-" | columnWidth <- columnWidths ]
  in  T.unlines
      $   T.intercalate " "
      .   zipWith3 format columnWidths separators
      <$> rows

width :: Cell -> Int
width Separator       = 0
width Empty           = 0
width (AlignLeft   t) = T.length t
width (AlignRight  t) = T.length t
width (AlignCenter t) = T.length t
width (IndentBy n t ) = n + T.length t

format :: Int -> Text -> Cell -> Text
format _ sep Separator       = sep
format n _   (AlignLeft   t) = T.justifyLeft n ' ' t
format n _   (AlignRight  t) = T.justifyRight n ' ' t
format n _   (AlignCenter t) = T.center n ' ' t
format n _   Empty           = T.replicate n " "
format n _   (IndentBy i t)  = T.replicate i " " <> T.justifyLeft (n - i) ' ' t

formatStandard :: Sum Scientific -> Text
formatStandard = T.pack . formatScientific Fixed (Just 2) . getSum

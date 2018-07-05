module Beans.Table where

import           Data.List                (transpose)
import           Data.Monoid              (Sum, getSum, (<>))
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Data.Text                (Text)
import qualified Data.Text                as T

-- a type for fill functions
type Filler = Int -> Text -> Text

-- a type for describing table columns
data ColDesc t = ColDesc
  { colTitleFill :: Filler
  , colTitle     :: Text
  , colValueFill :: Filler
  , colValue     :: t -> Text
  }

-- Table formatting inspired by:
-- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell

fillLeft, fillRight, fillCenter :: Char -> Int -> Text -> Text
fillLeft  = flip T.justifyLeft
fillRight = flip T.justifyRight
fillCenter = flip T.center

left, right, center :: Int -> Text -> Text
left = fillLeft ' '
right = fillRight ' '
center = fillCenter ' '

showTable :: [ColDesc item] -> [item] -> [item] -> Text
showTable coldefs items totals =
  let header = colTitle <$> coldefs
      rows = [[colValue coldef item | coldef <- coldefs] | item <- items]
      totalRows =
        [[colValue coldef footer | coldef <- coldefs] | footer <- totals]
      widths =
        [ maximum $ T.length <$> column
        | column <- transpose $ header : rows ++ totalRows
        ]
      separator = T.intercalate "-+-" [T.replicate width "-" | width <- widths]
      fillColumns fill columns =
        T.intercalate
          " | "
          [ fill c width column
          | (c, width, column) <- zip3 coldefs widths columns
          ]
   in T.unlines $
      fillColumns colTitleFill header :
      separator :
      (fillColumns colValueFill <$> rows) <> pure separator <>
      (fillColumns colValueFill <$> totalRows) <>
      pure separator


formatStandard :: Sum Scientific -> Text
formatStandard = T.pack . formatScientific Fixed (Just 2) . getSum

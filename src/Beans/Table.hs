module Beans.Table where

import           Data.List                (transpose)
import           Data.Monoid              (Sum, getSum, (<>))
import           Data.Scientific.Extended (FPFormat (Fixed), Scientific,
                                           formatScientific)
import           Data.Text                (Text)
import qualified Data.Text                as T

-- a type for align functions
type Filler = Int -> Text -> Text

-- a type for describing table columns
data Column t = Column
  { colAlignHeader :: Filler
  , colHeader     :: Text
  , colAlignValue :: Filler
  , colValue     :: t -> Text
  }

-- Table formatting inspired by:
-- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell

left, right, center :: Int -> Text -> Text
left = flip T.justifyLeft ' '
right = flip T.justifyRight ' '
center = flip T.center ' '

showTable :: [Column item] -> [item] -> Text
showTable coldefs rows =
  let header = colHeader <$> coldefs
      body = [[colValue coldef row | coldef <- coldefs] | row <- rows]
      widths =
        [maximum $ T.length <$> column | column <- transpose $ header : body]
      separator = T.intercalate "-+-" [T.replicate width "-" | width <- widths]
      alignColumns align columns =
        T.intercalate
          " | "
          [ align c width column
          | c <- coldefs
          | width <- widths
          | column <- columns
          ]
   in T.unlines $
      alignColumns colAlignHeader header :
      separator : (alignColumns colAlignValue <$> body) <> pure separator


formatStandard :: Sum Scientific -> Text
formatStandard = T.pack . formatScientific Fixed (Just 2) . getSum

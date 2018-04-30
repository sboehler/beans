module Haricot.Report.Table where

import           Data.List (intercalate, transpose)

-- a type for fill functions
type Filler = Int -> String -> String

-- a type for describing table columns
data ColDesc t = ColDesc { colTitleFill :: Filler
                         , colTitle     :: String
                         , colValueFill :: Filler
                         , colValue     :: t -> String
                         }

-- Table formatting inspired by:
-- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell

fillLeft, fillRight, fillCenter :: a -> Int -> [a] -> [a]

fillLeft c n s = s ++ replicate (n - length s) c
fillRight c n s = replicate (n - length s) c ++ s
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - length s
          l = x `div` 2
          r = x - l

left, right, center :: Int -> String -> String

left = fillLeft ' '
right = fillRight ' '
center = fillCenter ' '

showTable :: [ColDesc t] -> [t] -> String
showTable cs ts =
  let header = map colTitle cs
      rows = [[colValue c t | c <- cs] | t <- ts]
      widths = [maximum $ map length col | col <- transpose $ header : rows]
      separator = intercalate "-+-" [replicate width '-' | width <- widths]
      fillCols fill cols =
        intercalate
          " | "
          [fill c width col | (c, width, col) <- zip3 cs widths cols]
   in unlines $
      fillCols colTitleFill header :
      separator : map (fillCols colValueFill) rows

module Beans.Import.CH.Postfinance.Tests
  ( tests
  )
where

import           Beans.Import.CH.Postfinance              ( name
                                                          , parseEntries
                                                          )
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Text                                ( pack )
import           System.FilePath.Posix                    ( (</>) )
import           Test.Tasty                               ( TestTree
                                                          , testGroup
                                                          )
import           Test.Tasty.Golden                        ( goldenVsString )
import           Test.Tasty.HUnit                         ( assertEqual
                                                          , testCase
                                                          )

tests :: TestTree
tests = testGroup "CH.Postfinance" [test1, test2]

test1 :: TestTree
test1 = goldenVsString "Postfinance" goldenFile action
 where
  path       = "tests/Beans/Import/CH/Postfinance/"
  sourceFile = path </> "postfinance1.csv"
  goldenFile = path </> "postfinance1.golden"
  action     = BS.pack . unlines . fmap show <$> parseEntries sourceFile

test2 :: TestTree
test2 = testCase "name attribute" $ assertEqual
  "Correct name"
  Beans.Import.CH.Postfinance.name
  (pack "ch.postfinance")

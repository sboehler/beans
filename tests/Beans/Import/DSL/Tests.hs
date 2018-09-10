module Beans.Import.DSL.Tests where

import           Beans.Import.DSL
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text)
import           System.FilePath.Posix      ((<.>), (</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (goldenVsString)
import           Text.Megaparsec            (parse, parseErrorPretty)

mkTest :: (Show a) => String -> Parser a -> Text -> TestTree
mkTest testName parser input = goldenVsString testName fileName action
  where
    fileName = "tests/Beans/Import/DSL" </> testName <.> "golden"
    action =
      (return . BS.pack . either parseErrorPretty show . parse parser testName)
        input

mkGroup :: (Show a) => String -> Parser a -> [(Int, Text)] -> TestTree
mkGroup groupName parser cases =
  testGroup
    groupName
    [mkTest (groupName ++ show n) parser input | (n, input) <- cases]

tests :: TestTree
tests =
  testGroup "Beans.DSL.Parser" [boolExprTests, amountExprTests, rulesTests]

boolExprTests :: TestTree
boolExprTests =
  mkGroup
    "boolExpr"
    boolExpr
    [ (1, "true")
    , (2, "false")
    , (3, "(true)")
    , (4, "(valueDate > 2018-01-01) and (valueDate\n <= \n2018-10-01)")
    , (5, "(amount <= 4.0) and (amount >= 2) or (40 < 50)")
    , (6, "description ~=~ \"asdf.*\"")
    , (7, "not true or false and not false")
    ]

amountExprTests :: TestTree
amountExprTests =
  mkGroup
    "amountExpr"
    amountExpr
    [ (1, "amount")
    , (2, "4")
    , (3, "(4.0)")
    , (4, "((amount + 4) - 4.0)")
    , (5, "(amount - amount + amount)")
    ]

rulesTests :: TestTree
rulesTests =
  mkGroup
    "rules"
    rules
    [(1, "true-> Assets:Account;"), (2, "false-> Assets:Account;")]

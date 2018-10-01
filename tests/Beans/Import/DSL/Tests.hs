module Beans.Import.DSL.Tests where

import           Beans.Data.Accounts        (Account (..), AccountType (..),
                                             Commodity (..))
import           Beans.Import.Common        (Entry (..))
import           Beans.Import.DSL
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import           Data.Time.Calendar         (fromGregorian)
import           System.FilePath.Posix      ((<.>), (</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (goldenVsString)
import           Test.Tasty.HUnit           (assertEqual, testCase)
import           Text.Megaparsec            (parse, parseErrorPretty)

mkParserTest :: (Show a) => String -> Parser a -> Text -> TestTree
mkParserTest testName parser input = goldenVsString testName fileName action
 where
  fileName = "tests/Beans/Import/DSL" </> testName <.> "golden"
  action =
    (return . BS.pack . either parseErrorPretty show . parse parser testName)
      input

mkParserTestGroup :: (Show a) => String -> Parser a -> [(Int, Text)] -> TestTree
mkParserTestGroup groupName parser cases = testGroup
  groupName
  [ mkParserTest (groupName ++ show n) parser input | (n, input) <- cases ]

tests :: TestTree
tests = testGroup "DSL" [parserTests, evaluationTests]

parserTests :: TestTree
parserTests = testGroup
  "Beans.DSL.Parser"
  [boolExprTests, amountExprTests, rulesTests, textExprTests]
 where
  boolExprTests = mkParserTestGroup
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
  amountExprTests = mkParserTestGroup
    "amountExpr"
    amountExpr
    [ (1, "amount")
    , (2, "4")
    , (3, "(4.0)")
    , (4, "((amount + 4) - 4.0)")
    , (5, "(amount - amount + amount)")
    , (6, "(abs amount + 100)")
    ]
  textExprTests = mkParserTestGroup
    "textExpr"
    textExpr
    [ (1, "\"some text\"")
    , (2, "(\"some text within parens\")")
    , (3, "description")
    ]
  rulesTests = mkParserTestGroup
    "rules"
    rules
    [(1, "true-> Assets:Account;"), (2, "false -> Assets:Account;")]

evaluationTests :: TestTree
evaluationTests = testGroup "evaluation tests" (mkTest <$> evaluationTestCases)
 where
  entry = Entry (fromGregorian 2018 1 1)
                "Purchasing Foos at Bar Inc"
                (-100)
                (Commodity "CHF")
                (fromGregorian 2018 1 1)
                "zz.bigbank"
                (Just 500)
  account1 = Account Expenses ["Shopping"]
  account2 = Account Assets ["Bankaccount"]
  account3 = Account Liabilities ["Creditcard"]
  mkTest (rs, e) = testCase (show rs)
    $ assertEqual (" should evaluate to " <> show e) e (evaluate rs entry)
  evaluationTestCases =
    [ ( [Rule (EMatch EVarDescription (EText "Bar Inc")) account1]
      , Just account1
      )
    , ([Rule (EBool True) account2]                     , Just account2)
    , ([Rule (EGT EVarAmount (EAmount (-110))) account3], Just account3)
    , ([Rule (ELT EVarAmount (EAmount (-110))) account3], Nothing)
    , ( [ Rule (EEQ EVarDescription (EText "Purchasing Foos at Bar Inc"))
               account3
        ]
      , Just account3
      )
    , ( [Rule (EMatch EVarDescription (EText "^Purchasing")) account3]
      , Just account3
      )
    , ([Rule (EMatch EVarDescription (EText "^Foo")) account3], Nothing)
    , ([Rule (EOr (EBool True) (EBool False)) account3]       , Just account3)
    , ([Rule (EOr (EBool False) (EBool False)) account3]      , Nothing)
    , ([Rule (EOr (EBool False) (EBool True)) account3]       , Just account3)
    , ([Rule (EOr (EBool True) (EBool True)) account3]        , Just account3)
    , ( [Rule (EGT EVarBookingDate (EDate $ fromGregorian 2017 12 31)) account1]
      , Just account1
      )
    , ( [Rule (ELT EVarBookingDate (EDate $ fromGregorian 2017 12 31)) account1]
      , Nothing
      )
    ]

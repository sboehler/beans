module Beans.Import.CH.Postfinance.Tests
  ( tests
  )
where

import           Beans.Model                    ( Account(Account)
                                                , AccountType(..)
                                                )
import           Beans.Import.CH.Postfinance    ( name
                                                , parse
                                                )
import           Beans.Import.Common            ( Config(..) )
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Text                      ( pack )
import           System.FilePath.Posix          ( (</>) )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( goldenVsString )
import           Control.Monad.Reader           ( runReaderT )
import           Test.Tasty.HUnit               ( assertEqual
                                                , testCase
                                                )
import qualified Data.Text.Prettyprint.Doc     as P
import           Beans.Pretty                   ( )


tests :: TestTree
tests = testGroup "CH.Postfinance" [test1, test2]

test1 :: TestTree
test1 = goldenVsString "Postfinance" goldenFile action
 where
  path       = "tests/Beans/Import/CH/Postfinance/"
  sourceFile = path </> "postfinance1.csv"
  goldenFile = path </> "postfinance1.golden"
  action     = BS.pack . show . P.sep . fmap P.pretty <$> runReaderT
    parse
    (Config evaluate sourceFile (Account Assets ["Checking"]))




evaluate :: a -> Maybe Account
evaluate = const $ Just $ Account Expenses ["Errands"]


test2 :: TestTree
test2 = testCase "name attribute" $ assertEqual
  "Correct name"
  Beans.Import.CH.Postfinance.name
  (pack "ch.postfinance")

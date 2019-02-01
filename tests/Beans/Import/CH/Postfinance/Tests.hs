module Beans.Import.CH.Postfinance.Tests
  ( tests
  )
where

import           Beans.Import.CH.Postfinance    ( name
                                                , parse
                                                )
import           Beans.Import.Common            ( Config(..)
                                                , ImporterException(..)
                                                )
import           Beans.Model                    ( Account(Account)
                                                , AccountType(..)
                                                )
import           Beans.Pretty                   ( )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Text                      ( pack )
import qualified Data.Text.Prettyprint.Doc     as P
import           System.FilePath.Posix          ( (</>) )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( goldenVsString )
import           Test.Tasty.HUnit               ( assertEqual
                                                , testCase
                                                )


tests :: TestTree
tests = testGroup "CH.Postfinance" [test1, test2]

test1 :: TestTree
test1 =
  let path       = "tests/Beans/Import/CH/Postfinance/"
      sourceFile = path </> "postfinance1.csv"
      goldenFile = path </> "postfinance1.golden"
      config     = Config evaluate sourceFile (Account Assets ["Checking"])
  in  goldenVsString "Postfinance" goldenFile $ do
        res <- parse config <$> B.readFile sourceFile
        case res of
          Left (ImporterException a) -> return $ BS.pack a
          Right b -> return . BS.pack . show . P.sep . fmap P.pretty $ b


evaluate :: a -> Maybe Account
evaluate = const $ Just $ Account Expenses ["Errands"]

test2 :: TestTree
test2 = testCase "name attribute" $ assertEqual
  "Correct name"
  Beans.Import.CH.Postfinance.name
  (pack "ch.postfinance")

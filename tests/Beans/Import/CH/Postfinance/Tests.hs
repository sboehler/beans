module Beans.Import.CH.Postfinance.Tests
  ( tests,
  )
where

import Beans.Account (Account (Account), AccountType (..))
import Beans.Import.CH.Postfinance (parse)
import Beans.Import.Common (Config (..), ImporterException (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Prettyprint.Doc as P
import System.FilePath.Posix ((</>))
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests = testGroup "CH.Postfinance" [test1]

test1 :: TestTree
test1 =
  let path = "tests/Beans/Import/CH/Postfinance/"
      sourceFile = path </> "postfinance1.csv"
      goldenFile = path </> "postfinance1.golden"
      config = Config "ch.postfinance" sourceFile (Account Assets ["Checking"])
   in goldenVsString "Postfinance" goldenFile $ do
        res <- parse config <$> B.readFile sourceFile
        case res of
          Left (ImporterException a) -> return $ BS.pack a
          Right b -> return . BS.pack . show . P.sep . fmap P.pretty $ b

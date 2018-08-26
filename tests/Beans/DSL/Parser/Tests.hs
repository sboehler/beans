module Beans.DSL.Parser.Tests where

import           Beans.DSL.Parser
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text)
import           System.FilePath.Posix      ((<.>), (</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (goldenVsString)
import           Text.Megaparsec            (parseMaybe)

mkTest :: (Show a) => String -> Parser a -> Text -> TestTree
mkTest testName parser input = goldenVsString testName fileName action
  where
    fileName = "tests/Beans/DSL/Parser" </> testName <.> "golden"
    action = (return . BS.pack . show . parseMaybe parser) input

tests :: TestTree
tests = testGroup "Beans.DSL.Parser" [test1, test2]

test1, test2 :: TestTree
test1 = mkTest "First test" rules "true-> Assets:Account;"
test2 = mkTest "Second test" rules "false-> Assets:Account;"

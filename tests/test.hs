import qualified Beans.Import.DSL.Tests
import qualified Test.Tasty               as T
import           Test.Tasty.Golden.Manage (acceptingTests)


main :: IO ()
main = T.defaultMainWithIngredients ingredients tests
  where ingredients = acceptingTests : T.defaultIngredients

tests :: T.TestTree
tests = T.testGroup "Tests" [Beans.Import.DSL.Tests.tests]

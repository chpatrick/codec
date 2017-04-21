import           Test.Tasty
import           Test.Tasty.Ingredients.Basic (consoleTestReporter)

import           Data.Aeson.Codec.Test
import           Data.Binary.Codec.Test

main :: IO ()
main = defaultMainWithIngredients [ consoleTestReporter ] allTests

allTests :: TestTree
allTests =
  testGroup "Codec"
    [ aesonTests
    , binaryTests
    ]

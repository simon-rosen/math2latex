<<<<<<< HEAD
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "math2latex" $ do
    arithmatecSpec
    parenSpec
    bigExpressionsSpec


arithmatecSpec :: Spec
arithmatecSpec = do
  describe "arithmetic" $ do
    it "addition" $ do
      pending
    it "subtraction" $ do
      pending
    it "multiplication" $ do
      pending
    it "division" $ do
      pending

parenSpec :: Spec
parenSpec = do
  describe "parentheses" $ do
    it "nested" $ do
      pending
    it "multiplication with expression" $ do
      pending

bigExpressionsSpec :: Spec
bigExpressionsSpec = do
  describe "big expressions" $ do
    it "large complex expression" $ do
      pending
=======
main :: IO ()
main = putStrLn "Test suite not yet implemented"
>>>>>>> ab45cd1 (Changed documentation to the Haddock format.)

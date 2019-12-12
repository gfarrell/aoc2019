module UtilSpec (spec) where

import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "has" $ do
    it "returns True if an element is in a list" $ do
      shouldBe (has "a" ["c", "d", "a", "0"]) (True)
      shouldBe (has "d" ["c", "d", "a", "d"]) (True)
    it "returns False if an element is not in a list" $ do
      shouldBe (has 42 [1, 2, 3, 4, 5]) (False)

  describe "splice" $ do
    it "replaces an element at the given index" $ do
      shouldBe (splice 42 2 [1, 2, 3, 4, 5]) ([1, 2, 42, 4, 5])

  describe "lines_to_list" $ do
    it "converts a newline separated list of number to a list of Integers" $ do
      let txt = "12\n42\n96\n2712\n314159"
          exp = [12, 42, 96, 2712, 314159]
      lines_to_list txt `shouldBe` exp

module Day1Spec (spec) where

import Test.Hspec
import Day1

spec :: Spec
spec = do
  describe "mass_to_fuel" $ do
    it "returns 0 for masses under 6" $ do
      mass_to_fuel 4 `shouldBe` (0)
      mass_to_fuel 0 `shouldBe` (0)
      mass_to_fuel 6 `shouldBe` (0)

    it "returns the correct calc" $ do
      mass_to_fuel 12 `shouldBe` (2)
      mass_to_fuel 14 `shouldBe` (2)
      mass_to_fuel 1969 `shouldBe` (654)
      mass_to_fuel 100756 `shouldBe` (33583)

  describe "how_much_fuel" $ do
    it "returns 0 for an empty list" $ do
      how_much_fuel [] `shouldBe` (0)

    it "calculates correctly" $ do
      how_much_fuel [12] `shouldBe` (2)
      how_much_fuel [12, 14] `shouldBe` (4)
      how_much_fuel [24, 48, 96] `shouldBe` (50)

  describe "total_fuel" $ do
    it "calculates the amount of fuel, including to carry the fuel" $ do
      total_fuel 14 `shouldBe` (2)
      total_fuel 1969 `shouldBe` (966)
      total_fuel 100756 `shouldBe` (50346)

  describe "lines_to_list" $ do
    it "converts a newline separated list of number to a list of Integers" $ do
      let txt = "12\n42\n96\n2712\n314159"
          exp = [12, 42, 96, 2712, 314159]
      lines_to_list txt `shouldBe` exp

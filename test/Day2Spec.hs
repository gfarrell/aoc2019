module Day2Spec (spec) where

import Test.Hspec
import Day2

spec :: Spec
spec = do
  describe "run" $ do
    it "runs example programmes correctly" $ do
      shouldBe (run [1,9,10,3,2,3,11,0,99,30,40,50])
               (Just [3500,9,10,70,2,3,11,0,99,30,40,50])
      shouldBe (run [1,0,0,0,99]) (Just [2,0,0,0,99])
      shouldBe (run [2,3,0,3,99]) (Just [2,3,0,6,99])
      shouldBe (run [2,4,4,5,99,0]) (Just [2,4,4,5,99,9801])
      shouldBe (run [1,1,1,4,99,5,6,0,99]) (Just [30,1,1,4,2,5,6,0,99])

  describe "validate" $ do
    it "detects programmes with invalid opcodes" $ do
      shouldBe (validate [1, 2, 3, 4, 5, 99]) (False)
      shouldBe (validate [3, 1, 1, 1, 99]) (False)
    it "detects programmes with foreshortened arg lists" $ do
      shouldBe (validate [1, 2, 3, 1, 2, 1, 99]) (False)
      shouldBe (validate [1, 2, 99]) (False)
    it "detects programmes that fail to end on 99" $ do
      shouldBe (validate [1, 1, 1, 1, 99, 0]) (True)
      shouldBe (validate [1, 1, 1, 1, 1]) (False)
    it "validates correct programmes" $ do
      shouldBe (validate [1,9,10,3,2,3,11,0,99,30,40,50]) (True)
      shouldBe (validate [1,0,0,0,99]) (True)
      shouldBe (validate [2,3,0,3,99]) (True)
      shouldBe (validate [2,4,4,5,99,0]) (True)
      shouldBe (validate [1,1,1,4,99,5,6,0,99]) (True)

  describe "with_inputs" $ do
    it "replaces memory slots" $ do
      shouldBe (with_inputs (12, 2) [1, 1, 1, 1, 1]) ([1, 12, 2, 1, 1])

  describe "test_inputs" $ do
    it "runs a programme with given inputs and returns the value in #0" $ do
      let mem = [1, 0, 0, 0, 99, 6, 9, 12] in do
        shouldBe (test_inputs (4, 5) mem) (105)
        shouldBe (test_inputs (6, 3) mem) (9)
        shouldBe (test_inputs (6, 7) mem) (21)

  describe "seek" $ do
    it "finds the pair of inputs to a programme that give a target number" $ do
      let mem = [1, 0, 0, 3, 2, 3, 11, 0, 99, 30, 40, 50] in do
        shouldBe (seek 3500 mem) (Just (10, 9))

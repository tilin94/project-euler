module MultiplesOf3Or5Spec where

import Data.List             (nub)

import MyLib                 (m3OrM5LessThan)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck       (choose, forAll)

spec :: Spec
spec = do
  describe "m3OrM5LessThan" $ do

    prop "all returned values must be less than n"
      . forAll (choose (3, 100)) $ \n ->
      m3OrM5LessThan n `shouldSatisfy` all (< n)

    prop "all input values between -100 and 3 makes it return []"
      . forAll (choose (-100, 3)) $ \n ->

        let multiples = m3OrM5LessThan n
        in multiples `shouldBe` []

    prop "all the returned values must be multiples of either 3 or 5"
      . forAll (choose (3, 100)) $ \n ->
        let
          isMultipleOf3Or5 x = x `mod` 3 == 0 || x `mod` 5 == 0
        in m3OrM5LessThan n `shouldSatisfy` all isMultipleOf3Or5

    prop "all returned values are unique"
      . forAll (choose (3, 100)) $ \n ->
        let multiples = m3OrM5LessThan n
        in multiples `shouldSatisfy` (\xs -> length xs == length (nub xs))

    prop "all returned values are sorted incrementally"
      . forAll (choose (3, 100)) $ \n ->
        let
          multiples = m3OrM5LessThan n
          isSorted []       = True
          isSorted [_]      = True
          isSorted (x:y:xs) = x <= y && isSorted (y:xs)
        in multiples `shouldSatisfy` (isSorted)

    it "handles zero" $ do
      m3OrM5LessThan 0 `shouldBe` []

    it "handles 10" $ do
      m3OrM5LessThan 10 `shouldBe` [3,5,6,9]

    it "handles 20" $ do
      m3OrM5LessThan 20 `shouldBe` [3,5,6,9,10,12,15,18]

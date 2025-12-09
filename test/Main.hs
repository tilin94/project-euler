module Main (main) where

import qualified MultiplesOf3Or5Spec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Multiples of 3 or 5" MultiplesOf3Or5Spec.spec

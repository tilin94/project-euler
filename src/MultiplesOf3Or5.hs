module MultiplesOf3Or5 (multiplesOf3Or5) where

import Data.List (nub, sort)

multiplesOf3Or5 :: Int -> [Int]
multiplesOf3Or5 n
  | n < 3 = []
  | otherwise = sort . nub $ [ x | x <- [3,6..n] <> [5,10..n], x < n]

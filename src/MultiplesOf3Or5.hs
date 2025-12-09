module MultiplesOf3Or5 (m3OrM5LessThan) where

m3OrM5LessThan :: Int -> [Int]
m3OrM5LessThan n
  | n < 3 = []
  | otherwise =
      [ x
      | x <- [3..n-1]
      , x `mod` 3 == 0 || x `mod` 5 == 0 ]

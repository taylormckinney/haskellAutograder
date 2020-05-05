--123456789
module Mckinneytl where
 countZeros :: [Integer] -> Integer
 countZeros [] = 0
 countZeros (x:xs) | x == 0 = 1 + countZeros xs
                  | otherwise = countZeros xs

 printRes x = "there are " ++ (show x) ++ " zeros"

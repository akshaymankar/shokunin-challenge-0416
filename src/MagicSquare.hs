module MagicSquare (magicSquare, normalMagicSquare) where

import Data.List (tails, find, transpose)
import Data.Set (fromList, size)

normalMagicSquare :: Int -> Maybe [[Int]]
normalMagicSquare n = magicSquare n [1..(n * n)]

magicSquare :: Int -> [Int] -> Maybe [[Int]]
magicSquare n xs = if isInt expectedSum
                      then find (isMagicSquare xs) allSquares
                      else Nothing
                        where expectedSum = (fromIntegral (sum xs) :: Float) / (fromIntegral n :: Float)
                              allSquares = cartesianPower n $ magicRows n xs (ceiling expectedSum)

-- This doesn't verify the rows for optimization. #LacksPurity
isMagicSquare :: [Int] -> [[Int]] -> Bool
isMagicSquare xs xss = isMagicSumValid && containsAllNumbers
  where
    n = length xss - 1
    isMagicSumValid = size (fromList (map sum (diagonal1 n xss:diagonal2 n xss:transpose xss))) == 1
    containsAllNumbers = fromList (concat xss) == fromList xs

diagonal1 :: Int -> [[a]] -> [a]
diagonal1 0 xss = [head $ head xss]
diagonal1 n xss = (xss !! n !! n) : diagonal1 (n - 1) xss

diagonal2 :: Int -> [[a]] -> [a]
diagonal2 (-1) xss = []
diagonal2 n xss = (xss !! n !! (length xss - n - 1)) : diagonal2 (n - 1) xss

-- For the lack of better name
-- magic sets are sets of numbers which sum up to `(sum xs)/ (length xs)`
magicRows :: Int -> [Int] -> Int -> [[Int]]
magicRows n xs expectedSum = [ x | x <- cartesianPower n xs, sum x == expectedSum ]

-- Like cartesianProduct but n times and flattened
cartesianPower :: Int -> [a] -> [[a]]
cartesianPower n xs = foldl (\acc _ -> cartesianProductList xs acc) [[]] [1..n]

cartesianProductList :: [a] -> [[a]] -> [[a]]
cartesianProductList xs yss = [x:ys | x <- xs, ys <- yss]

isInt :: RealFrac a => a -> Bool
isInt a = ceiling a == floor a


module Day03 where
import Util
import Data.List

parseLineToBinVec :: String -> [Int]
parseLineToBinVec ('0':xs) = 0:parseLineToBinVec xs
parseLineToBinVec ('1':xs) = 1:parseLineToBinVec xs
parseLineToBinVec (x:xs) = parseLineToBinVec xs       -- No #trinarygang today
parseLineToBinVec []  = []

parseInputDay3 :: String -> [[Int]]
parseInputDay3 text = map (parseLineToBinVec) $ lines text

computeGammaEpsilon :: [[Int]] -> (Int, Int)
computeGammaEpsilon vectors = (gamma, epsilon)
                            where
                               ones = foldl1 (zipWith (+)) vectors
                               len = length vectors
                               zeroes = map (\n -> len-n) ones
                               gammaV = map fromEnum $ zipWith (>) ones zeroes
                               epsilonV = map (\n->1-n) gammaV
                               gamma = (toNum) gammaV
                               epsilon = (toNum) epsilonV

toNum :: [Int] -> Int
toNum numbers = foldl (\c b -> 2*c + b) 0 numbers

computeD3Q1 :: [[Int]] -> Int
computeD3Q1 l = gamma * epsilon
              where (gamma, epsilon) = computeGammaEpsilon l

answerD3Q1 = runOnFile computeD3Q1 parseInputDay3 "../data/2021_03/data"

-- Finds a rating given the binary numbers, a function from count of ones and count of zeroes to 1 or 0, and a carry
findRating :: [[Int]] -> (Int -> Int -> Int) -> [Int] -> Int
findRating [xs] _ carry = toNum $ (reverse carry) ++ xs
findRating nums fn carry = findRating newNums fn (bit:carry)
                    where
                      zeros = length $ filter (\r -> 0 == head r) $ nums
                      ones = (length nums) - zeros
                      bit = fn ones zeros
                      filteredNums = filter (\num -> head num == bit) nums
                      newNums = map tail filteredNums

o2rating :: Int -> Int -> Int
o2rating o z    | o < z = 0
                | o >= z = 1

co2rating :: Int -> Int -> Int
co2rating o z   | o >= z = 0
                | o < z = 1



computeD3Q2 :: [[Int]] -> Int
computeD3Q2 nums = o2r * co2r
                  where
                     o2r = findRating nums o2rating []
                     co2r = findRating nums co2rating []

answerD3Q2 = runOnFile computeD3Q2 parseInputDay3 "../data/2021_03/data"

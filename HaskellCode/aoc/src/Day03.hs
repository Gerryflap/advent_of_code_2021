module Day03 where
import Util

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
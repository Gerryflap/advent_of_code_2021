module Day06 where
import Util
import Data.List.Split
import Data.Map

decreaseAndAdd :: Int -> [Int] -> [Int]
decreaseAndAdd 0 [] = []
decreaseAndAdd new [] = 8:decreaseAndAdd (new-1) []
decreaseAndAdd new (x:xs)   | x == 0 = 6:decreaseAndAdd (new+1) xs
                            | otherwise = (x-1):decreaseAndAdd (new) xs

parseDay6 :: String -> [Int]
parseDay6 s = map read $ splitOn "," s

simulateDays :: Int -> [Int] -> [Int]
simulateDays 0 l = l
simulateDays n l = simulateDays (n-1) $ decreaseAndAdd 0 l

computeD6Q1 :: [Int] -> Int
computeD6Q1 l = length $ simulateDays 80 l

answerD6Q1 = runOnFile computeD6Q1 parseDay6 "../data/2021_06/data"

computeD6Q2 :: [Int] -> Int
computeD6Q2 l = length $ simulateDays 256 l

answerD6Q2 = runOnFile computeD6Q2 parseDay6 "../data/2021_06/test"
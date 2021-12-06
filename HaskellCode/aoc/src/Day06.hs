module Day06 where
import Util
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

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

computeD6Q1 :: [Int] -> Int;
computeD6Q1 l = length $ simulateDays 80 l

answerD6Q1 = runOnFile computeD6Q1 parseDay6 "../data/2021_06/data"

type LanternMap = M.Map Int Int

decreaseAndAddEfficient :: LanternMap -> LanternMap
decreaseAndAddEfficient m = snd $ foldl updateMapFor (m, M.empty) [0..8]

updateMapFor :: (LanternMap, LanternMap) -> Int -> (LanternMap, LanternMap)
updateMapFor (old, new) 0 = (old, updatedMap)
                          where
                            old0 = getCountVal $ M.lookup 0 old
                            tempMap = M.insert 6 old0 new
                            updatedMap = M.insert 8 old0 tempMap
updateMapFor (old, new) n = (old, updatedMap)
                          where
                            oldV = getCountVal $ M.lookup n old
                            newV = getCountVal $ M.lookup (n-1) new
                            updatedMap = M.insert (n-1) (oldV + newV) new

simulateDaysEfficient :: Int -> LanternMap -> LanternMap
simulateDaysEfficient 0 m = m
simulateDaysEfficient n m = simulateDaysEfficient (n-1) $ decreaseAndAddEfficient m

processD6Q2Input :: [Int] -> LanternMap -> LanternMap
processD6Q2Input [] m = m
processD6Q2Input (x:xs) m = processD6Q2Input xs $ M.insert x (newCountVal $ M.lookup x m) m

computeD6Q2 :: [Int] -> Int
computeD6Q2 l = sum $ map snd $ M.toList $ simulateDaysEfficient 256 $ processD6Q2Input l M.empty

answerD6Q2 = runOnFile computeD6Q2 parseDay6 "../data/2021_06/data"
module Day07 where
import Data.List
import Util
import Debug.Trace

median :: [Int] -> Int
median l  | length l `mod` 2 == 1 = med
          | otherwise = round medAvg
          where
            sorted = sort l
            med = sorted !! (length l `div` 2 - 1)
            med2 = sorted !! ((length l `div` 2 - 1) + 1)
            medAvg = (fromIntegral (med + med2)) / 2.0

mean :: [Int] -> Int
mean l = round $ (fromIntegral $ sum l) / (fromIntegral $ length l)

computeFuel :: Int -> [Int] -> Int
computeFuel pos crabs = sum $ map (\x -> abs (x-pos)) crabs

computeFuel2 :: Int -> [Int] -> Int
computeFuel2 pos crabs = sum $ map (\x -> f $ abs (x-pos)) crabs
                        where
                           f 0 = 0
                           f x = x + (f (x-1))

-- Given starting pos x, the previous minimum, the step/delta function and an input list,
--    keeps applying delta to x and computing the part 2 fuel until the fuel consumption goes up again, then returns
--    the min position and fuel. This works because computeFuel2 is convex.
computeOptimum2 :: Int -> Maybe Int -> (Int -> Int) -> [Int] -> (Int, Int)
computeOptimum2 x Nothing delta crabs = computeOptimum2 x (Just $ computeFuel2 x crabs) delta crabs
computeOptimum2 x (Just minV) delta crabs  | v <= minV  = computeOptimum2 (delta x) (Just v) delta crabs
                                          | otherwise = (x, minV)
                                          where
                                            v = computeFuel2 (delta x) crabs

-- Finds the minimum fuel usage starting from the mean position (which is very close) and descending the fuel cost curve
--  until an optimum is reached.
findD7Q2 :: [Int] -> Int
findD7Q2  crabs   | f1 < f2 = trace (show (m, x1)) f1
                  | otherwise = trace (show (m, x2)) f2
                  where
                    -- Start at the mean value
                    m = mean crabs
                    -- Search to the left/lower than the mean
                    (x1, f1) = computeOptimum2 m Nothing (\x -> (-) x 1) crabs
                    -- Search to the right/higher than the mean
                    (x2, f2) = computeOptimum2 m Nothing (+1) crabs

answerD7Q1 = runOnFile (\l -> computeFuel (median l) l) parseNumLine "../data/2021_07/data"
answerD7Q2 = runOnFile findD7Q2 parseNumLine "../data/2021_07/data"
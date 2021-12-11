module Day11 where
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Util
import Data.Maybe

neighbours = [(1,-1), (1, 0), (1, 1), (0, -1), (0, 1), (-1, -1), (-1, 0), (-1, 1)]

-- Converts 2D coordinate to 1D index IF the coordinate is in the bounds, otherwise it returns Nothing
toIndex :: (Int, Int) -> (Int, Int) -> Maybe Int
toIndex (width, height) (x,y)   | isInBounds = (Just index)
                                | otherwise = Nothing
                                where
                                  isInBounds = x >= 0 && x < width && y >= 0 && y < height
                                  index = x + y * width

-- Converts 1D index back to coordinates (UNSAFE: only use when you know index is in bounds)
fromIndex :: (Int, Int) -> Int -> (Int, Int)
fromIndex (width, height) i = (i `mod` width, i `div` width)

-- Given width/height, stack of flashes to process, list of all processed flashes so far this step and the vector
--    containing the energy levels, process all flashes for this step until the stack runs out and returns the
--    new energy level vector and the total list of flashes for this step. This function fully handles the
--    contagion of flashes. It assumes the "+1 step" and finding of initial flashes has already been performed.
processFlashes :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Vector Int -> (Vector Int, [(Int, Int)])
processFlashes wh [] flashes v = (v, flashes)
processFlashes wh ((x, y):flashStack) flashes v = processFlashes wh newFlashStack newFlashList nv
                                    where
                                      -- Find the Maybe 1D indices of all neighbouring tiles
                                      nbIndices = map (toIndex wh) $ map (\(dx, dy) -> (x+dx, y+dy)) neighbours
                                      -- Filters out all the invalid (out of bounds) neighbouring indices
                                      validNbs = map fromJust $ filter isJust nbIndices
                                      -- Enters the magic monad world to (+1) all energy levels of the valid neighbours
                                      nv = V.modify (\mv -> mapM_ (\i -> MV.modify mv (+1) i) validNbs) v
                                      -- Finds all neighbours that just flashed (and thus have level 10)
                                      newFlashes = map (fromIndex wh) $ filter (\i -> 10 == nv V.! i) validNbs
                                      -- Appends new flashes caused by our flash to the top of the stack
                                      newFlashStack = newFlashes ++ flashStack
                                      -- Appends new flashes caused by our flash to the list of all flashes this step
                                      newFlashList = newFlashes ++ flashes

-- Bumps all energy levels by 1. Returns new vector and a list of flashes caused by the bumping
bumpEnergy :: (Int, Int) -> Vector Int -> (Vector Int, [(Int, Int)])
bumpEnergy wh v = (nv, flashIndices)
              where
                nv = V.map (+1) v
                flashIndices = V.ifoldl collectFlash [] nv
                collectFlash l i 10 = (fromIndex wh i):l
                collectFlash l _ _ = l

-- One total step of flashing. bumps energy, processes flashes, and resets flashed energy to 0
flashStep :: (Int, Int) -> Vector Int -> (Vector Int, [(Int, Int)])
flashStep wh v = (nv, flashes)
                where
                  (v0, initFlashes) = bumpEnergy wh v
                  (v1, flashes) = processFlashes wh initFlashes initFlashes v0
                  nv = V.map resetEnergies v1

                  resetEnergies e | e > 9 = 0
                                  | otherwise = e

-- Step function for Q1. Does 1 step and adds new flashes to counter
stepD11Q1 :: (Int, Int) -> (Vector Int, Int) -> Int -> (Vector Int, Int)
stepD11Q1 wh (v, counter) _ = (nv, counter + length flashes)
                            where
                              (nv, flashes) = flashStep wh v

-- Does 100 steps of flashing and returns the total number of flashes that occurred
computeD11Q1 :: (Vector Int, (Int, Int)) -> Int
computeD11Q1 (v, wh) = snd $ foldl (stepD11Q1 wh) (v, 0) [1..100]

-- Parses Day 11 input to a 1D vector of energy levels and a (width,height) tuple
parseD11 :: String -> (Vector Int, (Int, Int))
parseD11 s = (v, wh)
            where
              ls = lines s
              wh = (length $ head ls, length ls)
              intMatrix :: [[Int]]
              intMatrix = map (map (\c -> read [c])) ls
              v = V.fromList $ foldl1 (++) intMatrix

-- Computes ✨THE ANSWER✨ to Q1 Day 11 from the input file
answerD11Q1 = runOnFile computeD11Q1 parseD11 "../data/2021_11/data"

-- Continues flash steps until the number of flashes that step equals the width*height (they all flash at the same time)
-- Design decision: steps input denotes the upcoming step, so to start off you need to input 1!
findD11Q2 :: (Int, Int) -> Vector Int -> Int -> Int
findD11Q2 wh@(w,h) v steps  | length flashes == w*h = steps
                            | otherwise = findD11Q2 wh nv (steps+1)
                            where
                              (nv, flashes) = flashStep wh v

-- Wrapper for D11 Q2 computation
computeD11Q2 :: (Vector Int, (Int, Int)) -> Int
computeD11Q2 (v, wh) = findD11Q2 wh v 1

-- Computes ✨THE ANSWER✨ to Q2 Day 11 from the input file
answerD11Q2 = runOnFile computeD11Q2 parseD11 "../data/2021_11/data"
-- Hello I'm the friendly comment at line 100. Have a nice day :)
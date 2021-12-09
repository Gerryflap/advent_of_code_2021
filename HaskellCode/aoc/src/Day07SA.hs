module Day07SA where
import Day07
import SimulatedAnnealing
import System.Random
import Util

{-
  This exists solely to test the Simulated Annealing implementation and to show it off
-}

day7Q1SAParams :: [Int] -> SimulatedAnnealingParams Int
day7Q1SAParams crabs = SAParams {
  s0 = 0,       -- Very silly starting point as all numbers are positive, but a good test for the technique
  seed = 420,   -- Best seed
  kMax = 1420,  -- Takes long, but 420 was sadly not enough for full convergence
  energyFn = (\pos -> fromIntegral $ computeFuel pos crabs),  -- Fuel expenditure is subject to minimization
  pAcceptanceFn = pAcceptanceDefault,
  tempFn = linearCooling,
  neighbourFn = selectNb 1  -- For now we consider numbers in range -1 to 1. This also includes 0, but idk
}

-- Override the energyFn with the part 2 one
day7Q2SAParams :: [Int] -> SimulatedAnnealingParams Int
day7Q2SAParams crabs = (day7Q1SAParams crabs) {energyFn = (\pos -> fromIntegral $ computeFuel2 pos crabs)}

-- Neighbour selection. This method returns a new Neighbour at "random" within (-range, range) inclusive
selectNb :: Int -> Int -> StdGen -> (Int, StdGen)
selectNb range state gen = (state + delta, newG)
                          where
                            (delta, newG) = uniformR (-range, range) gen

answerD7SAQ1 = runOnFile (\crabs -> computeFuel (simulatedAnnealing $ day7Q1SAParams crabs) crabs) parseNumLine "../data/2021_07/data"
answerD7SAQ2 = runOnFile (\crabs -> computeFuel2 (simulatedAnnealing $ day7Q2SAParams crabs) crabs) parseNumLine "../data/2021_07/data"
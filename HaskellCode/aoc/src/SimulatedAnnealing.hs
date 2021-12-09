module SimulatedAnnealing where
import System.Random

-- All parameters for the SA algo that you will need to supply.
-- IMPORTANT: Type "s" indicates the state type which is whatever you want it to be 🌈
data SimulatedAnnealingParams s = SAParams {
  -- Starting state s0
  s0 :: s,
  -- Seed int for random gen. Same seed will result in same outcome
  seed :: Int,
  -- Max number of steps, too small will not converge, too large will take long
  kMax :: Int,
  --Function to compute energy given state. The algorithm tries to minimize this value
  energyFn :: (s -> Double),
  -- Computes the chance of accepting the new state given: old energy, new energy, temperature
  pAcceptanceFn :: (Double -> Double -> Double -> Double),
  -- Temperature decay function. Takes k and kMax and returns temperature
  tempFn :: (Int -> Int -> Double),
  -- Neighbour function, should randomly generate a new neighbouring state from the given state and return the updated random gen
  neighbourFn :: (s -> StdGen -> (s, StdGen))
}

-- tempFn, Temperature linearly decays over kMax steps
linearCooling :: Int -> Int -> Double
linearCooling k kMax = 1.0 - ((fromIntegral k) + 1.0)/ (fromIntegral kMax)

-- Default p function, often used in SA. When e_new > e_old, P = 1, else P = exp (-(e_new - e_old)/t)
pAcceptanceDefault :: Double -> Double -> Double -> Double
pAcceptanceDefault e1 e2 t    | e2 < e1 = 1.0
                              | otherwise = exp (-(e2 - e1)/t)

-- Runs the SA algo with given parameters.
simulatedAnnealing :: SimulatedAnnealingParams s -> s
simulatedAnnealing params = fst $ foldl (simulatedAnnealingStep params) (s0 params, mkStdGen $ seed params) [1..kMax params]

-- Does a single SA step given parameters, current state, stgen and step (k).
simulatedAnnealingStep :: SimulatedAnnealingParams s -> (s, StdGen) -> Int -> (s, StdGen)
simulatedAnnealingStep params (s,g) k | p >= v = (candidateS, newG)
                                      | otherwise = (s, newG)
                                        where
                                          temp = tempFn params k (kMax params)
                                          (candidateS, g1) = neighbourFn params s g
                                          energyOld = energyFn params s
                                          energyCandi = energyFn params candidateS
                                          p = pAcceptanceFn params energyOld energyCandi temp
                                          (v, newG) = uniformR (0 :: Double, 1 :: Double) g1

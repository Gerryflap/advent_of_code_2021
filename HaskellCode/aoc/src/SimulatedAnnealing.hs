module SimulatedAnnealing where
import System.Random

data SimulatedAnnealingParams s = SAParams {
  s0 :: s,
  seed :: Int,
  kMax :: Int,
  energyFn :: (s -> Double),
  pAcceptanceFn :: (Double -> Double -> Double -> Double),
  tempFn :: (Int -> Int -> Double),
  neighbourFn :: (s -> StdGen -> (s, StdGen))
}

linearCooling :: Int -> Int -> Double
linearCooling k kMax = 1.0 - ((fromIntegral k) + 1.0)/ (fromIntegral kMax)



simulatedAnnealing :: SimulatedAnnealingParams s -> s
simulatedAnnealing params = fst $ foldl (simulatedAnnealingStep params) (s0 params, mkStdGen $ seed params) [1..kMax params]


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

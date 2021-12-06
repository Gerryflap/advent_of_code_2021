module Day06AltImpl where
import qualified Day06 as D6
import Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import Util

-- Performs a single day of fish stuff
decreaseAndAddAlt :: Vector Int -> Vector Int
decreaseAndAddAlt v = V.modify (\mv -> applyForN 7 (modVec mv v)) $ V.replicate 9 0

-- Haskell gods forgive me for what I'm about to do
-- Given Mutable vector newV, (immutable) copy oldV and index i, updates values for index i
-- Wrap-around updates for ages 0 to ages 6 and 8 are performed at i == 0
modVec :: PrimMonad m => MVector (PrimState m) Int -> Vector Int -> Int -> m ()
modVec newV oldV i  | i /= 0 = MV.modify newV (+oldV V.! (i+1)) i
                    | otherwise = write1 >> write2 >> write3
                    where
                      oldCount = oldV V.! 0
                      write1 = MV.modify newV (+oldV V.! 1) 0
                      write2 = MV.modify newV (+oldCount) 6
                      write3 = MV.modify newV (+oldCount) 8

-- Adds the counts of all ages to the count Vector
addAllD6 :: [Int] -> Vector Int
addAllD6 nums = V.modify (\mv -> applyForList nums (\num -> MV.modify mv (+1) num)) initialVec
            where
              initialVec = V.replicate 9 0

-- Simulates n days starting from Vector v
simulateDaysAlt :: Int -> Vector Int -> Vector Int
simulateDaysAlt 0 v = v
simulateDaysAlt n v = simulateDaysAlt (n-1) newV
                    where
                      newV = decreaseAndAddAlt v

computeD6Alt :: Int -> [Int] -> Int
computeD6Alt days l = V.sum $ simulateDaysAlt days $ addAllD6 l

answerD6Q1Alt = runOnFile (computeD6Alt 80) D6.parseDay6 "../data/2021_06/data"
answerD6Q2Alt = runOnFile (computeD6Alt 256) D6.parseDay6 "../data/2021_06/data"

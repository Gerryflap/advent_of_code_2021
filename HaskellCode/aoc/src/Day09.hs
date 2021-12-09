module Day09 where
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Util
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort)

findLows :: Matrix Int -> [((Int, Int), Int)]
findLows m = map fst $ filter snd $ Mx.toList $ Mx.mapPos (processMatrixLoc m) m

processMatrixLoc :: Matrix Int -> (Int, Int) -> Int -> (((Int, Int), Int), Bool)
processMatrixLoc m (x,y) v = (((x,y), v), isLowPoint)
                          where
                            neighbours = [(0,1),(1,0),(-1,0),(0,-1)]
                            nbVals = map fromJust $ filter isJust $ map (\(dx,dy) -> Mx.safeGet (x+dx) (y+dy) m) neighbours
                            isLowPoint = foldl (\b nbV -> b && (nbV > v)) True nbVals

computeD9Q1 :: Matrix Int -> Int
computeD9Q1 m = sum $ map (+1) $ map snd $ findLows m

parseD9 :: String -> Matrix Int
parseD9 s = Mx.fromLists $ map (map (\c -> read [c])) $ lines s

findAllInBasin :: [(Int, Int)] -> Set (Int, Int) -> Matrix Int -> Set (Int, Int)
findAllInBasin [] found m  = found
findAllInBasin ((x,y):xs) found m = findAllInBasin (xs ++ newCandidates) newFound m
                                  where
                                    newFound = S.insert (x,y) found
                                    neighbours = [(0,1),(1,0),(-1,0),(0,-1)]
                                    nbPos = map (\(dx, dy) -> (x+dx, y+dy)) neighbours
                                    inBasin = filter (neighbourInBasin m) nbPos
                                    newCandidates = filter (\e -> S.notMember e newFound) inBasin


neighbourInBasin :: Matrix Int -> (Int, Int) -> Bool
neighbourInBasin m (x,y) = v /= Nothing && v /= (Just 9)
                          where
                            v = Mx.safeGet x y m

computeD9Q2 :: Matrix Int -> Int
computeD9Q2 m = foldl1 (*) $ take 3 basins
            where
              lows = map fst $ findLows m
              basins = reverse $ sort $ map (\p -> S.size $ findAllInBasin [p] S.empty m) lows


answerD9Q1 = runOnFile computeD9Q1 parseD9 "../data/2021_09/data"

answerD9Q2 = runOnFile computeD9Q2 parseD9 "../data/2021_09/data"
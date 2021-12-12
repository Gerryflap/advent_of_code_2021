module Day12 where
import Util
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split
import Data.Char

-- Whether a node should be expanded according to normal (non-bonus) rules.
--      True when it's uppercase or lowercase and not yet visited
shouldExpand :: Set String -> String -> Bool
shouldExpand set str    | isLower (head str) = S.notMember str set
                        | otherwise = True

-- Adds string to set of visited small nodes if it's a lower case string
addToSet :: Set String -> String -> Set String
addToSet set str        | isLower (head str) = S.insert str set
                        | otherwise = set

-- Given the connections, the set of visited small caves, whether the "bonus" small cave was used, and the current pos:
--      return all paths from here
collectPaths :: Map String [String] -> Set String -> Bool -> String -> [[String]]
collectPaths _ _ _ "end" = [["end"]]
collectPaths m set usedBonus str  | usedBonus = map (str:) $ childPaths
                                  | otherwise = map (str:) $ childPaths ++ bonusChildPaths
                                  where
                                    nset = addToSet set str
                                    allNeighbours = M.findWithDefault [] str m
                                    nextNodes = filter (shouldExpand nset) allNeighbours
                                    childPaths = foldl (++) [] $ map (collectPaths m nset usedBonus) nextNodes
                                    -- For expanding small caves if we still have the "bonus"
                                    nextBonusNodes = filter (\s -> (not $ shouldExpand nset s) && s /= "start" ) allNeighbours
                                    bonusChildPaths = foldl (++) [] $ map (collectPaths m nset True) nextBonusNodes

-- Adds the pair of strings to the map such that the paths from s1 to s2 and s2 to s1 are added
addPairToMap :: Map String [String] -> (String, String) -> Map String [String]
addPairToMap m (s1, s2) = M.insertWith (++) s1 [s2] $ M.insertWith (++) s2 [s1] m

parseD12 :: String -> Map String [String]
parseD12 s = newMap
            where
              ls = lines s
              mappings = map (\l -> (l !! 0, l !! 1)) $ map (splitOn "-") ls
              newMap = foldl addPairToMap M.empty mappings

-- Q1 is now solved by just pretending like we've already used our "bonus" small cave
computeD12Q1 :: Map String [String] -> Int
computeD12Q1 m = length $ collectPaths m S.empty True "start"

answerD12Q1 = runOnFile computeD12Q1 parseD12 "../data/2021_12/data"

computeD12Q2 :: Map String [String] -> Int
computeD12Q2 m = length $ collectPaths m S.empty False "start"

answerD12Q2 = runOnFile computeD12Q2 parseD12 "../data/2021_12/data"
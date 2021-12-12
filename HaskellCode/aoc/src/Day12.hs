module Day12 where
import Util
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split
import Data.Char

shouldExpand :: Set String -> String -> Bool
shouldExpand set str    | isLower (head str) = S.notMember str set
                        | otherwise = True

addToSet :: Set String -> String -> Set String
addToSet set str        | isLower (head str) = S.insert str set
                        | otherwise = set

-- Given the connections, the set of visited small caves, and the current pos: return all paths from here
collectPaths :: Map String [String] -> Set String -> String -> [[String]]
collectPaths _ _ "end" = [["end"]]
collectPaths m set str = map (str:) childPaths
                        where
                          nset = addToSet set str
                          nextNodes = filter (shouldExpand nset) $ M.findWithDefault [] str m
                          childPaths = foldl (++) [] $ map (collectPaths m nset) nextNodes

addPairToMap :: Map String [String] -> (String, String) -> Map String [String]
addPairToMap m (s1, s2) = M.insertWith (++) s1 [s2] $ M.insertWith (++) s2 [s1] m

parseD12 :: String -> Map String [String]
parseD12 s = newMap
            where
              ls = lines s
              mappings = map (\l -> (l !! 0, l !! 1)) $ map (splitOn "-") ls
              newMap = foldl addPairToMap M.empty mappings

computeD12Q1 :: Map String [String] -> Int
computeD12Q1 m = length $ collectPaths m S.empty "start"

answerD12Q1 = runOnFile computeD12Q1 parseD12 "../data/2021_12/data"
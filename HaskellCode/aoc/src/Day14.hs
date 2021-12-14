module Day14 where
import Data.Map (Map)
import qualified Data.Map as M
import Util
import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

type PolymerMap = Map (Char, Char) Char

expandPolymer :: PolymerMap -> String -> String
expandPolymer _ [] = []
expandPolymer _ (c:[]) = [c]
expandPolymer m (x:y:xs)  | isJust mbe = x:e:(expandPolymer m (y:xs))
                          | otherwise = x:(expandPolymer m (y:xs))
                          where
                            mbe = M.lookup (x,y) m
                            e = fromJust mbe

parseD14 :: String -> (String, PolymerMap)
parseD14 s = (template, pairRules)
          where
            ls = lines s
            template = head ls
            pairRules = foldl parseD14PairRule M.empty (drop 2 ls)

-- Pls don't judge, it's late today :p
parseD14PairRule :: PolymerMap -> String -> PolymerMap
parseD14PairRule m (c1:c2:' ':'-':'>':' ':co:[]) = M.insert (c1,c2) co m
parseD14PairRule _ s = error $ "Cannot parse string as polymer pair insertion rule: \"" ++ s ++ "\""

countOccurrences :: String -> [(Char, Integer)]
countOccurrences s = M.toList $ foldl countOccurrence M.empty s
                    where
                      countOccurrence m c = M.insertWith (+) c 1 m

computeD14Q1 :: (String, PolymerMap) -> Integer
computeD14Q1 (template, pairRules) = answer
               where
                  outputStr = foldl (\s iter -> expandPolymer pairRules s) template [1..10]
                  occs = sortOn snd $ countOccurrences outputStr
                  answer = (snd $ last occs) - (snd $ head occs)

answerD14Q1 = runOnFile computeD14Q1 parseD14 "../data/2021_14/data"

-- >:(
type PairCountMap = Map (Char, Char) Integer

expandPolymer2 :: PolymerMap -> PairCountMap -> PairCountMap
expandPolymer2 polymap paircmap = foldl (processPolymer polymap) M.empty  $ M.toList paircmap

processPolymer :: PolymerMap -> PairCountMap -> ((Char, Char), Integer) -> PairCountMap
processPolymer polymap paircmap ((c1, c2), n) | isJust mbe = M.insertWith (+) (c1, e) n $ M.insertWith (+) (e, c2) n paircmap
                                              | otherwise = M.insertWith (+) (c1, c2) n paircmap
                                              where
                                                 mbe = M.lookup (c1, c2) polymap
                                                 e = fromJust mbe

toPairCounts :: String -> PairCountMap -> PairCountMap
toPairCounts [] m = m
toPairCounts [c] m = M.insertWith (+) (c, ' ') 1 m      -- We need this in order to be able to count the last one in the chain
toPairCounts (c1:c2:chars) m = M.insertWith (+) (c1, c2) 1 $ toPairCounts (c2:chars) m

toSingleElemCounts :: [((Char, Char), Integer)] -> Map Char Integer -> Map Char Integer
toSingleElemCounts [] m = m
toSingleElemCounts (((c1, c2), n):xs) m = M.insertWith (+) c1 n $ toSingleElemCounts xs m

computeD14Q2 :: (String, PolymerMap) -> Integer
computeD14Q2 (template, pairRules) = answer
                                    where
                                      outputMap = foldl (\m iter -> expandPolymer2 pairRules m) (toPairCounts template M.empty) [1..40]
                                      occs = sortOn snd $ M.toList $ toSingleElemCounts (M.toList outputMap) M.empty
                                      answer = (snd $ last occs) - (snd $ head occs)

answerD14Q2 = runOnFile computeD14Q2 parseD14 "../data/2021_14/data"
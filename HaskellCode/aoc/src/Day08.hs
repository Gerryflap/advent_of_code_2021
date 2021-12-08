module Day08 where
import Util
import Data.List.Split
import Data.Map (Map)
import Data.Set (Set, (\\), size)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

isSimpleDigit :: String -> Bool
isSimpleDigit s   | l == 2 = True
                  | l == 4 = True
                  | l == 3 = True
                  | l == 7 = True
                  | otherwise = False
                  where
                    l = length s

parseOnlyOutputs :: String -> [String]
parseOnlyOutputs s = flatten $ map words $ map (\line -> (splitOn "|" line) !! 1) $ lines s

answerD8Q1 = runOnFile (length.(filter isSimpleDigit)) parseOnlyOutputs "../data/2021_08/data"

getSimpleDigit :: Set Char -> Maybe Int
getSimpleDigit s  | l == 2 = Just 1
                  | l == 4 = Just 4
                  | l == 3 = Just 7
                  | l == 7 = Just 8
                  | otherwise = Nothing
                  where
                    l = size s

addSimpleDigit :: Map Int (Set Char) -> Set Char -> Map Int (Set Char)
addSimpleDigit m s  | md == Nothing = error $ "Could not parse simple digit " ++ show s
                    | otherwise = M.insert d s m
                    where
                      md = getSimpleDigit s
                      d = fromJust md

-- Assumed to be called AFTER addSimpleDigit
add6Digit :: Map Int (Set Char) -> Set Char -> Map Int (Set Char)
add6Digit m s | size (s \\ seven) == 4 = M.insert 6 s m
              | size (s \\ four) == 2 = M.insert 9 s m
              | otherwise = M.insert 0 s m
              where
                seven = fromJust $ M.lookup 7 m
                four = fromJust $ M.lookup 4 m

-- Assumed to be called AFTER add6Digit
add5Digit :: Map Int (Set Char) -> Set Char -> Map Int (Set Char)
add5Digit m s | size (s \\ one) == 3 = M.insert 3 s m
              | size (s \\ six) == 0 = M.insert 5 s m
              | otherwise = M.insert 2 s m
              where
                one = fromJust $ M.lookup 1 m
                six = fromJust $ M.lookup 6 m

deductNumbers :: [Set Char] -> Map Int (Set Char)
deductNumbers ws = finalMap
                  where
                    ezpz = filter (isJust.getSimpleDigit) ws
                    sixes = filter (\s -> 6 == size s) ws
                    fives = filter (\s -> 5 == size s) ws

                    simpleMap = foldl addSimpleDigit M.empty ezpz
                    sixesMap = foldl add6Digit simpleMap sixes
                    finalMap = foldl add5Digit sixesMap fives

finalizeMap :: Map Int (Set Char) -> Map String Int
finalizeMap m   | M.size m == 10 = M.fromList $ map (\(k, v) -> (S.toAscList v, k)) $ M.toList m
                | otherwise = error $ "Expected Map with 10 elements, got " ++ (show $ M.size m) ++ "for: " ++ (show m)

getDigitChar :: Map String Int -> String -> Char
getDigitChar m s = head $ show $ fromJust $ M.lookup (S.toAscList $ S.fromList s) m

computeOutputForLine :: String -> Int
computeOutputForLine s = read $ map (getDigitChar digitMap) outpWords
                        where
                          splitted = splitOn "|" s
                          inpWords = words $ head splitted
                          outpWords = words $ last splitted
                          digitMap = finalizeMap $ deductNumbers $ map (S.fromList) inpWords

computeD8Q2 :: [String] -> Int
computeD8Q2 ls = sum $ map computeOutputForLine ls

answerD8Q2 = runOnFile computeD8Q2 lines "../data/2021_08/data"

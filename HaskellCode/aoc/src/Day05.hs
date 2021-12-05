module Day05 where
import qualified Data.Map as Map
import Data.Char
import Util
import Debug.Trace

type Point = (Int, Int)
type Line = (Point, Point)
type PointMap = Map.Map (Int, Int) Int

-- ================ Computation =====================

-- Generates in-between points on an Integer grid between first and second point inclusive
generatePoints :: Point -> Point -> [Point]
generatePoints (x1, y1) (x2, y2) | x1 == x2 && y1 == y2 = [(x1, y1)]
                                          | otherwise = (x1, y1):generatePoints (x1+dx, y1+dy) (x2, y2)
                                          where
                                            dx = signum (x2 - x1)
                                            dy = signum (y2 - y1)

-- Computes the new counter value for a point in the map
newPointVal :: Maybe Int -> Int
newPointVal Nothing = 1             -- Point not in map, set to 1
newPointVal (Just x) = x + 1          -- Point in map, add 1 to counter

-- Counts the given point in the map and returns the updated map
addPoint :: PointMap -> Point -> PointMap
addPoint pmap point = Map.insert point newVal pmap
                       where
                        newVal = newPointVal $ Map.lookup point pmap

-- Adds all points that the line crosses to the PointMap
addLine :: PointMap -> Line -> PointMap
addLine pmap (p1, p2) = foldl addPoint pmap $ generatePoints p1 p2

-- Converts given lines to filled map
addLines :: [Line] -> PointMap
addLines = foldl addLine Map.empty

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

computeD5Q1 :: [Line] -> Int
computeD5Q1 ls = length twoOrMore
                where
                    twoOrMore = filter (\kv -> 1 < (snd kv)) $ Map.toList $ addLines $ filter isHorizontalOrVertical ls

computeD5Q2 :: [Line] -> Int
computeD5Q2 ls = length twoOrMore
                where
                    twoOrMore = filter (\kv -> 1 < (snd kv)) $ Map.toList $ addLines ls


-- ================== Parsing =======================
data Day05Token = IntVal Int
    | Comma
    | Arrow
    deriving (Show)

tokenizer :: String -> [Day05Token]
tokenizer [] = []
tokenizer (' ':remainder) = tokenizer remainder
tokenizer chars     | intVal /= [] = intValParsed:(tokenizer intRemainder)
                    | commaVal /= [] = Comma:(tokenizer commaRemainder)
                    | arrowVal /= [] = Arrow:(tokenizer arrowRemainder)
                    | otherwise = error $ "Could not tokenize input: '" ++ chars ++ "'"
                    where
                        (intVal, intRemainder) = tokenizeInt chars
                        intValParsed = IntVal $ read intVal
                        (commaVal, commaRemainder) = tokenizeComma chars
                        (arrowVal, arrowRemainder) = tokenizeArrow chars

tokenizeInt [] = ([], [])
tokenizeInt (x:xs)  | isNumber x    = ((x:cont), remainder)
                    | otherwise = ([], x:xs)
                     where
                       (cont, remainder) = tokenizeInt xs

tokenizeComma (',':xs) = (",", xs)
tokenizeComma _ = ([], [])

tokenizeArrow ('-':'>':xs) = ("->", xs)
tokenizeArrow _ = ([], [])

parseLineDefinitionFromToken :: [Day05Token] -> Line
parseLineDefinitionFromToken ((IntVal x1):Comma:(IntVal y1):Arrow:(IntVal x2):Comma:(IntVal y2):[]) = ((x1, y1), (x2, y2))
parseLineDefinitionFromToken _ = error "Cannot parse input"

parseLineFile :: String -> [Line]
parseLineFile s = map parseLineDefinitionFromToken $ map tokenizer $ lines s

answerD5Q1 = runOnFile computeD5Q1 parseLineFile "../data/2021_05/data"
answerD5Q2 = runOnFile computeD5Q2 parseLineFile "../data/2021_05/data"
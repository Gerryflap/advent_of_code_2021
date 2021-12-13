module Day13 where
import Data.Set (Set)
import qualified Data.Set as S
import Data.List.Split
import Util

data FoldInstr  = IUp Int
                | ILeft Int
                deriving (Show)

-- Translates the input coordinates according to the fold instruction
foldPoint :: FoldInstr -> (Int, Int) -> (Int, Int)
foldPoint (IUp yf) (x,y)     | y > yf = (x, yf - (y - yf))
                            | otherwise = (x,y)
foldPoint (ILeft xf) (x,y)   | x > xf = (xf - (x - xf), y)
                            | otherwise = (x,y)

-- Performs the given fold instruction over the entire set of points
executeFoldInstr :: Set (Int, Int) -> FoldInstr -> Set (Int, Int)
executeFoldInstr s instr = S.map (foldPoint instr) s

-- Performs all fold instructions in sequence over the given set
executeFoldInstrList :: Set (Int, Int) -> [FoldInstr] -> Set (Int, Int)
executeFoldInstrList set instrList = foldl executeFoldInstr set instrList

-- Parses a single line from the input file and returns the fold instruction
parseFoldInstr :: String -> FoldInstr
parseFoldInstr s  | axis == 'x' = ILeft v
                  | axis == 'y' = IUp v
                  | otherwise = error $ "Cannot parse expression: " ++ s
                  where
                    spl = splitOn "=" s
                    (part1, part2) = (spl !! 0, spl !! 1)
                    axis = last part1
                    v = read part2

-- Parses today's input
parseD13 :: String -> (Set (Int, Int), [FoldInstr])
parseD13 s = (set, instrList)
            where
              ls = splitOn [""] $ lines s
              (coords, instructions) = (ls !! 0, ls !! 1)
              set = S.fromList $ map (\l -> (read $ l !! 0, read $ l !! 1)) $ map (splitOn ",") coords
              instrList = map parseFoldInstr instructions

computeD13Q1 :: (Set (Int, Int), [FoldInstr]) -> Int
computeD13Q1 (s, fis) = S.size $ executeFoldInstr s (head fis)

answerD13Q1 = runOnFile computeD13Q1 parseD13 "../data/2021_13/data"

-- Renders the points to a multi-line String. The dimensions of this String will be equal to the max values of each axis
renderD13Q2 :: Set (Int, Int) -> String
renderD13Q2 set = renderD13Q2Helper (0,0) (mx,my) set
                where
                  mx = S.findMax $ S.map fst set
                  my = S.findMax $ S.map snd set

-- Helper function that actually does the rendering. Recursively visits all coordinates within the given bounds (mx, my)
renderD13Q2Helper :: (Int, Int) -> (Int, Int) -> Set (Int, Int) -> String
renderD13Q2Helper (x,y) (mx, my) set  | y > my = []
                                      | x > mx = '\n':renderD13Q2Helper (0, y+1) (mx,my) set
                                      | otherwise = symbol:renderD13Q2Helper (x+1, y) (mx,my) set
                                      where
                                        symbol = getSymbol $ S.member (x,y) set
                                        getSymbol b   | b = '■'
                                                      | otherwise = ' '

computeD13Q2 :: (Set (Int, Int), [FoldInstr]) -> IO ()
computeD13Q2 (s, fis) = putStr $ renderD13Q2 $ executeFoldInstrList s fis

answerD13Q2 = runOnFileIO computeD13Q2 parseD13 "../data/2021_13/data"
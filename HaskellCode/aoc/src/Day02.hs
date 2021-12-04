module Day02 where
import Util

data Direction = Forward Int
  | Down Int
  | Up Int

-- Q1
computeDepthAndX :: (Int, Int) -> Direction -> (Int, Int)
computeDepthAndX (depth, x) ((Forward y)) = (depth, x+y)
computeDepthAndX (depth, x) ((Down y)) = (depth+y, x)
computeDepthAndX (depth, x) ((Up y)) = (depth-y, x)

computeD2Q1 :: [Direction] -> Int
computeD2Q1 directions = depth * x
            where
               (depth, x) = foldl computeDepthAndX (0,0) directions

-- Q2
computeDepthAimAndX :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
computeDepthAimAndX (depth, aim, x) ((Forward y)) = (depth + aim*y, aim, x+y)
computeDepthAimAndX (depth, aim, x) ((Down y)) = (depth, aim+y, x)
computeDepthAimAndX (depth, aim, x) ((Up y)) = (depth, aim-y, x)

computeD2Q2 :: [Direction] -> Int
computeD2Q2 directions = depth * x
            where
               (depth, aim, x) = foldl computeDepthAimAndX (0,0,0) directions

parseInput :: String -> [Direction]
parseInput text = map parseLine textLines
                where
                  textLines = lines text

-- General stuff
parseLine :: String -> Direction
parseLine text  | w1 == "forward" = Forward (read w2)
                | w1 == "down" = Down (read w2)
                | w1 == "up"  = Up (read w2)
                where
                  ws = words text
                  w1 = head ws
                  w2 = last ws

answerD2Q1 = runOnFile computeD2Q1 parseInput "../data/2021_02/data_q1"
answerD2Q2 = runOnFile computeD2Q2 parseInput "../data/2021_02/data_q1"
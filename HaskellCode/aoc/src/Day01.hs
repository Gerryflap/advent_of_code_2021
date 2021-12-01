module Day01 where
  import Util

  countIncreases :: Ord a => [a] -> Int
  countIncreases numbers = foldl checkEntry 0 $ zip (init numbers) (tail numbers)

  checkEntry :: Ord a => Int -> (a, a) ->  Int
  checkEntry carry (x, y)   | x < y = carry + 1
                            | otherwise = carry

  conv3 :: Num a => [a] -> [a]
  conv3 [] = []
  conv3 [x] = [x]
  conv3 [x,y] = [x,y]
  conv3 l = zipWith (+) (init $ init l) $ zipWith (+) (init $ tail l) (tail $ tail l)

  findAnswerDay1q1 = runOnFile countIncreases parseLinesToInts "../data/2021_01/data"
                  where parseLinesToInts = parseLinesToType :: (String -> [Int])

  findAnswerDay1q2 = runOnFile (countIncreases.conv3) parseLinesToInts "../data/2021_01/data"
                  where parseLinesToInts = parseLinesToType :: (String -> [Int])
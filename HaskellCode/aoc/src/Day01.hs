module Day01 where
  countIncreases :: Ord a => [a] -> Int
  countIncreases numbers = foldl checkEntry 0 $ zip (init numbers) (tail numbers)

  checkEntry :: Ord a => Int -> (a, a) ->  Int
  checkEntry carry (x, y)   | x < y = carry + 1
                            | otherwise = carry

  computeAnswer1FromFile :: String -> Int
  computeAnswer1FromFile contents = answer
                                  where
                                    contentLines = lines contents
                                    numbers = map read contentLines :: [Int]
                                    answer = countIncreases numbers

  computeAnswer2FromFile :: String -> Int
  computeAnswer2FromFile contents = answer
                                  where
                                    contentLines = lines contents
                                    numbers = map read contentLines :: [Int]
                                    answer = countIncreases $ conv3 numbers
  conv3 :: Num a => [a] -> [a]
  conv3 [] = []
  conv3 [x] = [x]
  conv3 [x,y] = [x,y]
  conv3 l = zipWith (+) (init $ init l) $ zipWith (+) (init $ tail l) (tail $ tail l)

  findAnswerDay1q1 = do
    contents <- readFile "../data/2021_01/data"
    putStrLn $ show $ computeAnswer1FromFile contents

  findAnswerDay1q2 = do
    contents <- readFile "../data/2021_01/data"
    putStrLn $ show $ computeAnswer2FromFile contents
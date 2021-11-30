module PreviousYear where
  import Data.List
  import Util

  findNumbers :: (Num a, Ord a) => [a] -> Maybe (a,a)
  findNumbers numbers = findNumbersS sorted
              where sorted = sort numbers

  -- Finds two numbers that add up to 2020, assuming input is sorted
  findNumbersS :: (Num a, Ord a) => [a] -> Maybe (a,a)
  findNumbersS [] = Nothing
  findNumbersS (x:xs)   | index == -1 = findNumbersS xs
                        | otherwise = Just (x, y)
                        where
                          y = 2020 - x
                          index = binSearch xs y

  computeAnswerFromFile :: String -> Maybe Integer
  computeAnswerFromFile contents = answer
                                  where
                                    contentLines = lines contents
                                    numbers = map read contentLines
                                    answer = (\(x,y)->x*y) <$> findNumbers numbers

  findAnswerDay1 = do
    contents <- readFile "../data/2020_01/numbers"
    putStrLn $ show $ computeAnswerFromFile contents
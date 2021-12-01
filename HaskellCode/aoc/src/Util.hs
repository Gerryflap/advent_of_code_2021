module Util where
  -- Returns index or -1 if element cannot be found, assumes sorted input
  binSearch :: (Ord a, Num a) => [a] -> a -> Int
  binSearch xs y = binSearchH xs y 0

  -- Helper fn, takes a start index as well in order to be able to return an index
  binSearchH :: (Ord a, Num a) => [a] -> a -> Int -> Int
  binSearchH [] _ _ = -1
  binSearchH xs y i  | x == y = i
                    | x > y = binSearchH smaller y i
                    | otherwise = binSearchH larger y (i + (length smaller) + 1)
                    where
                      (smaller, (x:larger)) = splitAt (div (length xs) 2) xs

  -- Parses lines in the file to the desired type (type may have to be hinted with ::)
  parseLinesToType :: Read a => String -> [a]
  parseLinesToType text = map read $ lines text

  -- Runs the computationFn on an input file and prints the results
  -- Arguments:
  --  computationFn: gets input type a and returns b
  --  parser: Parses the String of the input file to input type a
  --  file path
  runOnFile :: (Read a, Show b) => (a -> b) -> (String -> a) -> String -> IO ()
  runOnFile computationFn parser path = do
      contents <- readFile path
      putStrLn $ show $ computationFn $ parser contents
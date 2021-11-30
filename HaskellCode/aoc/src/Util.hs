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

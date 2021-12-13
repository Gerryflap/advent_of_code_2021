module Util where
import Data.List.Split

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

parseNumLine :: String -> [Int]
parseNumLine s = map read $ splitOn "," s


flatten :: [[a]] -> [a]
flatten = foldl1 (++)

-- Runs the computationFn on an input file and prints the results
-- Arguments:
--  computationFn: gets input type a and returns b
--  parser: Parses the String of the input file to input type a
--  file path
runOnFile :: (Show b) => (a -> b) -> (String -> a) -> String -> IO ()
runOnFile computationFn parser path = do
    contents <- readFile path
    putStrLn $ show $ computationFn $ parser contents

-- Runs the computationFn on an input file and let's the computationFn do the printing
runOnFileIO :: (a -> IO ()) -> (String -> a) -> String -> IO ()
runOnFileIO computationFn parser path = do
    contents <- readFile path
    computationFn $ parser contents

-- Computes the new counter value for a counter in a map
newCountVal :: Maybe Int -> Int
newCountVal Nothing = 1               -- Key not in map, set counter to 1
newCountVal (Just x) = x + 1          -- Key in map, add 1 to counter

getCountVal :: Maybe Int -> Int
getCountVal Nothing = 0
getCountVal (Just x) = x

-- Applies the function for all i 0..N and returns nothing wrapped in the Monad
applyForN :: Monad m => Int -> (Int -> m ()) -> m ()
applyForN n fn = applyForList [0..n] fn

-- Applies the function for all i 0..N and returns nothing wrapped in the Monad
applyForList :: Monad m => [a] -> (a -> m ()) -> m ()
applyForList [] fn = return ()
applyForList (x:xs) fn = applyForList xs fn >> fn x
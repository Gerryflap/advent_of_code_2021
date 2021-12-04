module Day04 where
import Data.Vector (Vector, fromList, update)
import qualified Data.Vector as V
import Data.Maybe
import Data.List
import Data.List.Split
import Util
import Debug.Trace

type BingoCard = Vector ( Vector Int )
type DrawnMap = Vector ( Vector Bool )
type BoardState = (BingoCard, DrawnMap)

-- Checks row for bingo
checkRow :: DrawnMap -> Int -> Bool
checkRow drawn row = foldl1 (&&) $ drawn V.! row

-- Checks column for bingo
checkCol :: DrawnMap -> Int -> Bool
checkCol drawn col = foldl (\eq row -> eq && (row V.! col)) True drawn

-- Combines Maybe type such that the first Just will be returned or Nothing when no Just is found
combineMaybe :: Maybe a -> Maybe a -> Maybe a
combineMaybe Nothing x = x
combineMaybe (Just x) _ = Just x

-- Finds row and column for element, if it can be found. Otherwise returns Nothing
findRowCol :: BingoCard -> Int -> Maybe (Int, Int)
findRowCol bingoCard element = V.foldl1 combineMaybe $ V.imap (findInRow element) bingoCard

-- Finds element in row. Given element (to find), current row number, and the row Vector
-- Returns Just (row, col) or Nothing
findInRow :: Int -> Int -> Vector Int -> Maybe (Int, Int)
findInRow element rowNum row = (\col -> (rowNum, col)) <$> V.elemIndex element row

-- Sets that element at (row, col) has been drawn
setDrawn :: (Int, Int) -> DrawnMap -> DrawnMap
setDrawn (row,col) drawnMap = update drawnMap $ fromList [(row, updatedRowValues)]
                                where
                                    updatedRowValues = V.update (drawnMap V.! row) $ fromList [(col, True)]

-- Makes a game step. Given the board state and the drawn number, returns new boardstate and whether a bingo is detected
step :: Int -> BoardState -> (BoardState, Bool)
step drawn boardstate   | updated = ((bingoCard, updatedDrawnMap), isBingo)
                        | otherwise = (boardstate, False)
                        where
                            (bingoCard, drawnMap) = boardstate
                            maybeRowCol = findRowCol bingoCard drawn
                            updated = isJust maybeRowCol
                            (row, col) = fromJust maybeRowCol
                            updatedDrawnMap = setDrawn (row, col) drawnMap
                            isBingo = (checkRow updatedDrawnMap row) || (checkCol updatedDrawnMap col)

-- Makes a game step on all boards, returns updated boards and Maybe the board that has bingo
stepAll :: [BoardState] -> Int -> ([BoardState], Maybe BoardState)
stepAll boards drawn = (newBoards, bingo)
                        where
                            stepResults = map (step drawn) boards
                            (newBoards, _) = unzip stepResults
                            bingo = (\(board, _) -> board) <$> find (\(_,bool)->bool) stepResults

-- Given initial boardstates and a list of ints, draws the Ints until a bingo happens and then returns
--      the winning board and the last drawn number
findBingo :: [BoardState] -> [Int] -> (BoardState, Int)
findBingo boards [] = error "No bingo found"
findBingo boards (drawn:nums)   | isBingo = (bingoBoard, drawn)
                                | otherwise = findBingo newBoards nums
                                where
                                    (newBoards, maybeBingoBoard) = stepAll boards drawn
                                    isBingo = isJust maybeBingoBoard
                                    bingoBoard = fromJust maybeBingoBoard

-- Computes the sum of numbers on the card that have not been drawn yet, times the drawn number
computeResultFromBingo :: (BoardState, Int) -> Int
computeResultFromBingo ((bingoBoard, drawnMap), drawn) = summed * drawn
                                                        where
                                                            summed = V.sum rowSums
                                                            rowSums = V.map rowSum $ V.zip bingoBoard drawnMap

-- Computes the sum of undrawn numbers for a single row
rowSum :: (Vector Int, Vector Bool) -> Int
rowSum (bb, dm) = foldl addElem 0 $ V.zip bb dm

-- Adds num to the carry if it hasn't been drawn yet, otherwise it returns the carry + 0
addElem :: Int -> (Int, Bool) -> Int
addElem carry (value, isDrawn)  | isDrawn = carry
                                | otherwise = carry + value

-- Generates a new DrawnMap with same dimensions as BingoCard and with only False as values
newDrawnMap :: BingoCard -> DrawnMap
newDrawnMap = V.map $ V.map (\ _ -> False)

-- Parses the bingo file from a single String (all file contents)
parseBingoFileString :: String -> ([Int], [BoardState])
parseBingoFileString s = parseBingoFile $ lines s

-- Parses bingo file from lines. First line as comma separated numbers, the rest as bingo cards
parseBingoFile :: [String] -> ([Int], [BoardState])
parseBingoFile ls = (numbers, boards)
                where
                    numbers = map read $ splitOn "," $ head ls
                    cards = parseCards $ tail ls
                    boards = zip cards (map newDrawnMap cards)

-- Parses the remainder of the file as a list of lines (Strings) and returns a list of BingoCards
parseCards :: [String] -> [BingoCard]
parseCards [] = []
parseCards [""] = []
parseCards l    | lcard /= [] = (card:(parseCards remainder))
                | otherwise = parseCards remainder
                where
                    (lcard, remainder) = parseCard l
                    card = fromList lcard

-- Parses a single BingoCard and returns the card as a list of Vector Int, also returns the remaining lines
-- May also return ([], []) in some cases to be edgy. It's just a phase, please ignore it.
parseCard :: [String] -> ([Vector Int], [String])
parseCard [] = ([], [])
parseCard ("":lns) = ([], lns)
parseCard (ln:lns) = (row:rows, remainder)
                    where
                        row = fromList $ map read $ words ln
                        (rows, remainder) = parseCard lns

-- Computes the answer to D4Q1 given the input numbers and boardstates
computeD4Q1 :: ([Int], [BoardState]) -> Int
computeD4Q1 (numbers, boardstates) = computeResultFromBingo bingo
                                    where
                                        bingo = findBingo boardstates numbers

-- Computes the answer to D4Q1 by parsing the file and computing the result
answerD4Q1 = runOnFile computeD4Q1 parseBingoFileString "../data/2021_04/data"

-- Does a game step on all boards. Returns remaining lists (that didn't win) and winning lists,
stepAllRetList :: [BoardState] -> Int -> ([BoardState], [BoardState])
stepAllRetList boards drawn = (remainingBoards, bingos)
                        where
                            stepResults = map (step drawn) boards
                            bingos = map (\(board, _) -> board) $ filter (\(_,bool)->bool) stepResults
                            remainingBoards = map (\(board, _) -> board) $ filter (\(_,bool)->not bool) stepResults


--Finds the last board to have bingo
findLastBingo :: [BoardState] -> [Int] -> (BoardState, Int)
findLastBingo boards [] = error "Could not find a single last card before running out of numbers"
findLastBingo boards (drawn:nums)   | oneBoardRemaining = (head bingoBoards, drawn)
                                    | otherwise = findLastBingo newBoards nums
                                     where
                                        (newBoards, bingoBoards) = stepAllRetList boards drawn
                                        oneBoardRemaining = (0 == length newBoards) && (1 == length bingoBoards)

-- Computes D4Q2 for the input numbers and boards
computeD4Q2 :: ([Int], [BoardState]) -> Int
computeD4Q2 (numbers, boardstates) = computeResultFromBingo bingo
                                    where
                                        bingo = findLastBingo boardstates numbers

-- Computes the answer to D4Q2 by parsing the file and computing the result
answerD4Q2 = runOnFile computeD4Q2 parseBingoFileString "../data/2021_04/data"

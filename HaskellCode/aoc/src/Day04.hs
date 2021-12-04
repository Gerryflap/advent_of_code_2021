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

banaan :: BingoCard
banaan = fromList $ map fromList $ [[1,2,3], [3,4,5]]

checkRow :: DrawnMap -> Int -> Bool
checkRow drawn row = foldl1 (&&) $ drawn V.! row

checkCol :: DrawnMap -> Int -> Bool
checkCol drawn col = foldl (\eq row -> eq && (row V.! col)) False drawn

-- Combines Maybe type such that the first Just will be returned or Nothing when no Just is found
combineMaybe :: Maybe a -> Maybe a -> Maybe a
combineMaybe Nothing x = x
combineMaybe (Just x) _ = Just x

--setNumber :: BoardState -> Int -> (BoardState, Bool)
--setNumber boardstate drawn = (updatedBoard, modified)
--                           where
--                            (updatedBoardz, rowsModified) = V.unzip $ V.map (updateRow drawn) $ V.zip boardstate
--                            updatedBoard = V.unzip boardstate
--                            modified = foldl1 (&&) rowsModified

findRowCol :: BingoCard -> Int -> Maybe (Int, Int)
findRowCol bingoCard element = V.foldl1 combineMaybe $ V.imap (findInRow element) bingoCard


findInRow :: Int -> Int -> Vector Int -> Maybe (Int, Int)
findInRow element rowNum row = (\col -> (rowNum, col)) <$> V.elemIndex element row

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

computeResultFromBingo :: (BoardState, Int) -> Int
computeResultFromBingo ((bingoBoard, drawnMap), drawn) = summed * drawn
                                                        where
                                                            summed = V.sum rowSums
                                                            rowSums = V.map rowSum $ V.zip bingoBoard drawnMap
rowSum :: (Vector Int, Vector Bool) -> Int
rowSum (bb, dm) = foldl addElem 0 $ V.zip bb dm

addElem :: Int -> (Int, Bool) -> Int
addElem carry (value, isDrawn)  | isDrawn = carry
                                | otherwise = carry + value


newDrawnMap :: BingoCard -> DrawnMap
newDrawnMap = V.map $ V.map (\ _ -> False)

parseBingoFileString :: String -> ([Int], [BoardState])
parseBingoFileString s = parseBingoFile $ lines s

parseBingoFile :: [String] -> ([Int], [BoardState])
parseBingoFile ls = (numbers, boards)
                where
                    numbers = map read $ splitOn "," $ head ls
                    cards = parseCards $ tail ls
                    boards = zip cards (map newDrawnMap cards)

parseCards :: [String] -> [BingoCard]
parseCards [] = []
parseCards [""] = []
parseCards l    | lcard /= [] = (card:(parseCards remainder))
                | otherwise = parseCards remainder
                where
                    (lcard, remainder) = parseCard l
                    card = fromList lcard

parseCard :: [String] -> ([Vector Int], [String])
parseCard [] = ([], [])
parseCard ("":lns) = ([], lns)
parseCard (ln:lns) = (row:rows, remainder)
                    where
                        row = fromList $ map read $ words ln
                        (rows, remainder) = parseCard lns

computeD4Q1 :: ([Int], [BoardState]) -> Int
computeD4Q1 (numbers, boardstates) = computeResultFromBingo bingo
                                    where
                                        bingo = findBingo boardstates numbers

answerD4Q1 = runOnFile computeD4Q1 parseBingoFileString "../data/2021_04/data"
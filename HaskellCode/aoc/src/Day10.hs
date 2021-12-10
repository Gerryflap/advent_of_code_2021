module Day10 where
import Util
import Data.List
import Debug.Trace

data ParseResult
  = OK
  | Corrupted Char
  | Incomplete String
  | PNothing
  deriving (Show)

openBrackets = "([{<"
closeBrackets = ")]}>"

syntaxError :: ParseResult -> Bool
syntaxError OK = False
syntaxError (Corrupted _) = True
syntaxError (Incomplete _) = False
syntaxError PNothing = False

getClosing :: Char -> Char
getClosing '(' = ')'
getClosing '[' = ']'
getClosing '{' = '}'
getClosing '<' = '>'

parseChunk :: String -> (ParseResult, String)
parseChunk []       = (PNothing, [])
parseChunk (c:cs)   | c `elem` openBrackets = (resFinal, rem3)
                    | otherwise = (PNothing, (c:cs))
                    where
                      (res1, rem1) = parseChunk cs
                      (res2, rem2) = parseClosing (getClosing c) rem1
                      (res3, rem3) = parseChunk rem2
                      resFinal = res1 <✨> res2 <✨> res3


parseClosing :: Char -> String -> (ParseResult, String)
parseClosing pc []      = (Incomplete [pc], [])
parseClosing pc (c:cs)  | c == pc =(OK, cs)
                        | otherwise = (Corrupted c, cs)

getScoreQ1 :: ParseResult -> Int
getScoreQ1 (Corrupted ')') = 3
getScoreQ1 (Corrupted ']') = 57
getScoreQ1 (Corrupted '}') = 1197
getScoreQ1 (Corrupted '>') = 25137
getScoreQ1 _ = 0

answerD10Q1 = runOnFile (\ls -> sum $ map (getScoreQ1.fst) $ map parseChunk ls) lines "../data/2021_10/data"

-- Time to use some magic ✨
(<✨>) :: ParseResult -> ParseResult -> ParseResult
(<✨>) (Corrupted c) _ = Corrupted c
(<✨>) _ (Corrupted c) = Corrupted c
(<✨>) (Incomplete s1) (Incomplete s2) = (Incomplete (s1 ++ s2))
(<✨>) (Incomplete s) _ = (Incomplete s)
(<✨>) _ (Incomplete s) = (Incomplete s)
(<✨>) OK _ = OK
(<✨>) _ OK = OK
(<✨>) x y = y

getCharScore :: Char -> Integer
getCharScore ')' = 1
getCharScore ']' = 2
getCharScore '}' = 3
getCharScore '>' = 4

getScoreQ2 :: ParseResult -> Integer -> Integer
getScoreQ2 (Incomplete []) score = score
getScoreQ2 (Incomplete (c:cs)) score = getScoreQ2 (Incomplete cs) $ score * 5 + (getCharScore c)
getScoreQ2 _ _ = 0

computeD10Q1 :: [String] -> Integer
computeD10Q1 ls = scoresSorted !! (div (length scoresSorted) 2)
                where
                  parseResults = map (\l -> fst $ parseChunk l) ls
                  scores = filter (/=0) $ map (\r -> getScoreQ2 r 0) parseResults
                  scoresSorted = sort scores

answerD10Q2 = runOnFile computeD10Q1 lines "../data/2021_10/data"

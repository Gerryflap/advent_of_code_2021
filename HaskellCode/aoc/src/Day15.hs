module Day15 where
import Util
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQ
import Data.Maybe

type HeuristicFn = (Int, Int) -> (Int, Int) -> Maybe Integer
type FMap = Map (Int, Int) Integer
type GMap = Map (Int, Int) Integer
type CameFromMap = Map (Int, Int) (Int, Int)
type OpenSet = PQ.MinQueue (Integer, (Int, Int))

data AStarParams = ASParams {
  startPoint :: (Int, Int),
  widthHeight :: (Int, Int),
  endPoint :: (Int, Int),     -- In case a sneaky is pulled
  costs :: Matrix Int,
  heuristic :: HeuristicFn
}

data AStarState = ASState {
  openSet :: OpenSet,
  fMap :: FMap,
  gMap :: GMap,
  cfMap :: CameFromMap
}



-- Heuristic, computes manhattan distance
h1 :: HeuristicFn
h1 wh@(w,h) (x,y) =  (abs (w - x)) + (abs (h - y))

-- TODO: Proper init
aStarSearch :: AStarParams ->  AStarState
aStarSearch params = aStartSearch' params $ ASState {openSet=PQ.empty, fMap=Map.empty, gMap=Map.empty, cfMap=Map.empty}

aStarSearch' :: AStarParams -> AStarState -> AStarState
aStarSearch' params state  | PQ.size (openSet state) == 0 = error "Could not find the goal state"
                            | endPoint params == c_pos = state
                            | otherwise = aStarSearch' params state'
                            where
                              (current@(c_score, c_pos), openSetV1) = PQ.deleteFindMin (openSet state)
                              state1 = state {openSet = openSetV1}
                              state' = foldl (processNeighbour params current) state1  $ getNbD15 params c_pos


getScore :: Map (Int, Int) Int -> (Int, Int) -> Integer
getScore mp pos   | v == Nothing = -1
                  | otherwise = fromJust v
                  where
                     v = Map.lookup pos mp

getGScore state pos = getScore (gMap state) pos
getFScore state pos = getScore (fMap state) pos

processNeighbour :: AStarParams -> (Integer, (Int, Int)) -> AStarState -> (Integer, (Int, Int)) -> AStarState
processNeighbour params (cv, cpos@(cx, cy)) state (nbv, nbpos@(nbx, nby)) = state'
                                            where
                                              tentativeGScore = (getGScore state cpos) + nbv
                                              nbGscore = getGScore state nbpos
                                              -- For updating state (only if new gScore is better)
                                              fScore = tentativeGScore + heuristic params (widthHeight params) nbpos
                                              state' = state {
                                                cfMap = Map.insert nbpos cpos $ cfMap state,
                                                gMap = Map.insert nbpos tentativeGScore $ gMap state,
                                                fMap = Map.insert nbpos fScore $ fMap state,
                                                openSet = PQ.insert (fScore, nbpos) $ openSet state
                                              }



getNbD15 :: AStarParams -> (Int, Int) -> [(Integer, (Int, Int))]
getNbD15 params (x,y) = map (\(val, pos) -> (fromJust val, pos)) $ filter (isJust.fst) results
                          where
                            positions = map (\(dx,dy) -> (x+dx, y+dy)) [(0,1), (1,0), (0,-1), (-1, 0)]
                            results = map (\(x,y) -> (Mat.safeGet y x (costs params), (x,y))) positions


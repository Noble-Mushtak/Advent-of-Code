import Data.Bits
import Data.Char
import Data.Maybe
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.PriorityQueue.FingerTree as P
import qualified Deque.Lazy as D
import GHC.Exts
import Debug.Trace
--import Control.Exception

keys :: [Char]
keys = ['a'..'z']

data Cell = Wall | Empty | Key Int | Door Int deriving (Show, Eq)
type Grid = A.Array Int (A.Array Int Cell)

parseLine :: String -> [Cell]
parseLine = map parseCell
  where parseCell '#' = Wall
        parseCell '.' = Empty
        parseCell '@' = Empty
        parseCell ch
          | (ch >= 'a') && (ch <= 'z') = Key $ parseKey ch
          | (ch >= 'A') && (ch <= 'Z') = Door $ parseKey $ toLower ch
          | otherwise = error $ ch : " is not a recognized cell"

        parseKey = fromJust . flip L.elemIndex keys

createGrid :: [[Cell]] -> Grid
createGrid cellLists = A.listArray (0, length cellLists-1) $
                         map createRow cellLists
  where createRow cellList = A.listArray (0, length cellList-1) cellList

type Location = (Int, Int)

findInitialLoc :: [String] -> Location
findInitialLoc = fromJust .
                   foldr updateLocation
                         Nothing .
                         concat . map (zip [0..]) . zipWith (map . (,)) [0..]
  where updateLocation _ (Just loc) = Just loc
        updateLocation (i, (j, ch)) Nothing
          | ch == '@' = Just (i, j)
          | otherwise = Nothing

parseInput :: [String] -> (Grid, Location)
parseInput = (,) <$> (createGrid . map parseLine) <*> findInitialLoc

cellAtLoc :: Grid -> Location -> Cell
cellAtLoc g (i, j) = (g A.! j) A.! i

desiredBitset :: Int
desiredBitset = (2 ^ length keys)-1

findShortestPath :: Grid -> Location -> Int -> Int -> [(Int, Location)]
findShortestPath grid initialLoc keyBitset desiredKey =
  findShortestPath' S.empty $ fromList [(0, initialLoc)]
  where validCell Wall = False
        validCell Empty = True
        validCell (Key _) = True
        validCell (Door keyNeeded) = keyBitset `testBit` keyNeeded
        
        findShortestPath' visitedLocs locsDeque =
          case (D.uncons locsDeque) of
            Nothing -> []
            
            Just (curTuple@(curSteps, curLoc@(curX, curY)), rstQueue)
              | cellAtLoc grid curLoc == Key desiredKey -> [curTuple]
              
              | otherwise ->
                let newLocs = [(curX-1, curY),
                               (curX+1, curY),
                               (curX, curY-1),
                               (curX, curY+1)]
                    validLocs =
                      filter (validCell . cellAtLoc grid) newLocs
                    unvisitedLocs =
                      filter (not . flip S.member visitedLocs) validLocs

                    newVisited = foldr S.insert visitedLocs unvisitedLocs
                    newTuples = map ((,) (curSteps+1)) unvisitedLocs
                    newDeque = foldr D.snoc rstQueue newTuples in
                  findShortestPath' newVisited newDeque

data State = State { getLoc :: Location, getBitset :: Int } deriving (Show, Eq, Ord)

calcAnswer :: (Grid, Location) -> Maybe Int
calcAnswer (grid, initialLoc) =
  findShortestPathToSuccess S.empty $ P.fromList [(0, State initialLoc 0)]
  where findShortestPathToSuccess visitedStates statesQueue =
          case (P.minViewWithKey statesQueue) of
            Nothing -> Nothing
            
            Just ((curSteps, curState@(State curLoc curBitset)), rstQueue)
              | curBitset == desiredBitset -> Just curSteps
                  
              | S.member curState visitedStates ->
                findShortestPathToSuccess visitedStates rstQueue
                
              | otherwise ->
                traceShow (curSteps, cellAtLoc grid curLoc, popCount curBitset) $
                let possibleNextKeys =
                      filter (not . testBit curBitset) [0..length keys-1]
                      
                    newStates = do
                      nextKey <- possibleNextKeys
                      (numSteps, newLoc) <-
                        findShortestPath grid curLoc curBitset nextKey
                      return
                        (curSteps+numSteps,
                         State newLoc $ curBitset `setBit` nextKey)

                    newVisited = S.insert curState visitedStates
                    newQueue = foldr (uncurry P.add) rstQueue newStates in
                  findShortestPathToSuccess newVisited newQueue

main :: IO ()
main = interact ((++"\n") . show . calcAnswer . parseInput . lines)
-- main = interact ((++"\n") . show . length . filter ((<=) 3 . fst) . map ((,) =<< popCount . getBitset . read) . lines)
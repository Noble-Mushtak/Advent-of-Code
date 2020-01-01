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

toArray :: [a] -> A.Array Int a
toArray lst = A.listArray (0, length lst-1) lst

type Location = (Int, Int)

findInitialLocs :: [String] -> [Location]
findInitialLocs = foldr updateLocations
                       [] .
                       concat . map (zip [0..]) . zipWith (map . (,)) [0..]
  where updateLocations (i, (j, ch)) locsSoFar
          | ch == '@' = (i, j) : locsSoFar
          | otherwise = locsSoFar

type LocArray = A.Array Int Location

parseInput :: [String] -> (Grid, LocArray)
parseInput = (,) <$> (toArray . map (toArray . parseLine))
                 <*> (toArray . findInitialLocs)

cellAtLoc :: Grid -> Location -> Cell
cellAtLoc g (i, j) = (g A.! j) A.! i

desiredBitset :: Int
desiredBitset = (2 ^ length keys)-1

findAllKeys :: Grid -> Location -> [Int]
findAllKeys grid initialLoc =
  findAllKeys' S.empty [initialLoc] []
  where findAllKeys' _ [] keysFound = keysFound
        findAllKeys' visitedLocs (curLoc@(curX, curY):rstLocs) keysFound =
          let newKeys = case (cellAtLoc grid curLoc) of
                          Key newKey -> newKey : keysFound
                          _          -> keysFound
                          
              newLocs = [(curX-1, curY),
                         (curX+1, curY),
                         (curX, curY-1),
                         (curX, curY+1)]
              validLocs =
                filter ((/= Wall) . cellAtLoc grid) newLocs
              unvisitedLocs =
                filter (not . flip S.member visitedLocs) validLocs

              newVisited = S.insert curLoc visitedLocs
              newToBeVisited = unvisitedLocs ++ rstLocs in
            findAllKeys' newVisited newToBeVisited newKeys

type KeyMap = A.Array Int Int

createKeyMap :: (Grid, LocArray) -> KeyMap
createKeyMap (grid, initialLocs) =
  A.array (0, length keys-1) allKeyPairs
  where (minSection, maxSection) = A.bounds initialLocs

        allKeyPairs = do
            section <- [minSection..maxSection]
            key <- findAllKeys grid $ initialLocs A.! section
            return (key, section)

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

data State = State LocArray Int deriving (Show, Eq, Ord)

calcAnswer :: (Grid, LocArray) -> KeyMap -> Maybe Int
calcAnswer (grid, initialLocs) keyMap = 
  findShortestPathToSuccess S.empty $ P.fromList [(0, State initialLocs 0)]
  where findShortestPathToSuccess visitedStates statesQueue =
          case (P.minViewWithKey statesQueue) of
            Nothing -> Nothing
            
            Just ((curSteps, curState@(State curLocs curBitset)), rstQueue)
              | curBitset == desiredBitset -> Just curSteps
                  
              | S.member curState visitedStates ->
                findShortestPathToSuccess visitedStates rstQueue
                
              | otherwise ->
                traceShow (curSteps, curLocs, popCount curBitset) $
                let possibleNextKeys =
                      filter (not . testBit curBitset) [0..length keys-1]
                      
                    newStates = do
                      nextKey <- possibleNextKeys
                      
                      let nextKeySection = keyMap A.! nextKey
                      let curLoc = curLocs A.! nextKeySection
                      (numSteps, newLoc) <-
                        findShortestPath grid curLoc curBitset nextKey

                      let newLocs = curLocs A.// [(nextKeySection, newLoc)]
                      let newBitset = curBitset `setBit` nextKey
                      return (curSteps+numSteps, State newLocs newBitset)

                    newVisited = S.insert curState visitedStates
                    newQueue = foldr (uncurry P.add) rstQueue newStates in
                  findShortestPathToSuccess newVisited newQueue

main :: IO ()
main = interact ((++"\n") . show . (flip calcAnswer =<< createKeyMap) . parseInput . lines)
-- main = interact ((++"\n") . show . length . filter ((<=) 3 . fst) . map ((,) =<< popCount . getBitset . read) . lines)
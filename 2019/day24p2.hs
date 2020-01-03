import qualified Data.Array as A
import Control.Monad
import Data.Maybe

input :: [String]
input = ["#####", ".#.#.", ".#..#", "....#", "..###"]

toArray :: [a] -> A.Array Int a
toArray lst = A.listArray (0, length lst-1) lst

data Cell = Empty | Bug deriving (Show, Eq, Ord)
type Grid = A.Array Int (A.Array Int Cell)

parseCell :: Char -> Cell
parseCell '#' = Bug
parseCell '.' = Empty
parseCell _   = error "Unrecognized character"

parseInput :: [String] -> Grid
parseInput = toArray . map (toArray . map parseCell)

type ThreeDGrid = A.Array Int Grid
type Location = (Int, Int, Int)

createThreeDGrid :: Int -> Grid -> ThreeDGrid
createThreeDGrid maxLevel levelZeroGrid =
  A.listArray (-maxLevel, maxLevel) $ map createLevel [-maxLevel..maxLevel]
  where (minX, maxX) = A.bounds levelZeroGrid
        (minY, maxY) = A.bounds $ levelZeroGrid A.! minX

        emptyGrid = A.listArray (minX, maxX) $
                      repeat $
                        A.listArray (minY, maxY) $ repeat Empty
        
        createLevel 0 = levelZeroGrid
        createLevel _ = emptyGrid

cellAtLoc :: ThreeDGrid -> Location -> Maybe Cell
cellAtLoc grid (curLevel, curX, curY)
  | (curLevel >= minLevel) && (curLevel <= maxLevel) &&
      (curX >= minX) && (curX <= maxX) &&
      (curY >= minY) && (curY <= maxY) =
    Just $ ((grid A.! curLevel) A.! curX) A.! curY
  | otherwise = Nothing
  where (minLevel, maxLevel) = A.bounds grid
        (minX, maxX) = A.bounds $ grid A.! minLevel
        (minY, maxY) = A.bounds $ (grid A.! minLevel) A.! minX

findNeighbors :: Location -> [Location]
findNeighbors (level, x, y)
  | (x, y) == (2, 2) = []
  | (x, y) == (1, 2) = [(level, x-1, y),
                        (level, x, y-1),
                        (level, x, y+1)] ++
                       map ((,,) (level+1) 0) [0..4]
  | (x, y) == (3, 2) = [(level, x+1, y),
                        (level, x, y-1),
                        (level, x, y+1)] ++
                       map ((,,) (level+1) 4) [0..4]
  | (x, y) == (2, 1) = [(level, x-1, y),
                        (level, x+1, y),
                        (level, x, y-1)] ++
                       map ((,,) (level+1) `flip` 0) [0..4]
  | (x, y) == (2, 3) = [(level, x-1, y),
                        (level, x+1, y),
                        (level, x, y+1)] ++
                       map ((,,) (level+1) `flip` 4) [0..4]
  | (x, y) == (0, 0) = [(level-1, 1, 2),
                        (level-1, 2, 1),
                        (level, x, y+1),
                        (level, x+1, y)]
  | (x, y) == (0, 4) = [(level-1, 1, 2),
                        (level-1, 2, 3),
                        (level, x, y-1),
                        (level, x+1, y)]
  | (x, y) == (4, 0) = [(level-1, 3, 2),
                        (level-1, 2, 1),
                        (level, x, y+1),
                        (level, x-1, y)]
  | (x, y) == (4, 4) = [(level-1, 3, 2),
                        (level-1, 2, 3),
                        (level, x, y-1),
                        (level, x-1, y)]
  | x == 0           = [(level-1, 1, 2),
                        (level, x, y-1),
                        (level, x, y+1),
                        (level, x+1, y)]
  | x == 4           = [(level-1, 3, 2),
                        (level, x, y+1),
                        (level, x, y-1),
                        (level, x-1, y)]
  | y == 0           = [(level-1, 2, 1),
                        (level, x-1, y),
                        (level, x+1, y),
                        (level, x, y+1)]
  | y == 4           = [(level-1, 2, 3),
                        (level, x-1, y),
                        (level, x+1, y),
                        (level, x, y-1)]
  | otherwise        = [(level, x-1, y),
                        (level, x+1, y),
                        (level, x, y-1),
                        (level, x, y+1)]

updateThreeDGrid :: ThreeDGrid -> ThreeDGrid
updateThreeDGrid curThreeDGrid =
  A.listArray (minLevel, maxLevel) $ map genGrid [minLevel..maxLevel]
  where (minLevel, maxLevel) = A.bounds curThreeDGrid
        (minX, maxX) = A.bounds $ curThreeDGrid A.! minLevel
        (minY, maxY) = A.bounds $ (curThreeDGrid A.! minLevel) A.! minX

        genGrid curLevel = A.listArray (minX, maxX) $
                             map (genRow curLevel) [minX..maxX]

        genRow curLevel curX = A.listArray (minY, maxY) $
                        map (genCell curLevel curX) [minY..maxY]

        genCell curLevel curX curY =
          let neighborLocs = findNeighbors (curLevel, curX, curY)
              neighborCells = neighborLocs >>=
                                (maybeToList . cellAtLoc curThreeDGrid)
              numBugNeighbors = sum $ map (fromEnum . (== Bug)) neighborCells in
            case (cellAtLoc curThreeDGrid (curLevel, curX, curY)) of
              Just Empty
                | (numBugNeighbors == 1) || (numBugNeighbors == 2) -> Bug
                | otherwise                                        -> Empty
              Just Bug
                | numBugNeighbors == 1 -> Bug
                | otherwise            -> Empty
                
              Nothing -> error $
                           "Could not read the given location: " ++
                             show (curX, curY)

countBugs :: ThreeDGrid -> Int
countBugs grid = length $ filter (==Just Bug) $ map (cellAtLoc grid) allLocs
  where (minLevel, maxLevel) = A.bounds grid
        (minX, maxX) = A.bounds $ grid A.! minLevel
        (minY, maxY) = A.bounds $ (grid A.! minLevel) A.! minX
        
        allLocs = (,,) <$> [minLevel..maxLevel]
                       <*> [minX..maxX]
                       <*> [minY..maxY]

main :: IO ()
main = flip forM_ print $ take 201 $ map countBugs $ iterate updateThreeDGrid $ createThreeDGrid 200 $ parseInput input
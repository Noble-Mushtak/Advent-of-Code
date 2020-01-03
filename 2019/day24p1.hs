import qualified Data.Array as A
import qualified Data.Set as S
import Data.Maybe

input :: [String]
input = ["#####", ".#.#.", ".#..#", "....#", "..###"]

toArray :: [a] -> A.Array Int a
toArray lst = A.listArray (0, length lst-1) lst

data Cell = Empty | Bug deriving (Show, Eq, Ord)
type Grid = A.Array Int (A.Array Int Cell)
type Location = (Int, Int)

cellAtLoc :: Grid -> Location -> Maybe Cell
cellAtLoc grid (curX, curY)
  | (curX >= minX) && (curX <= maxX) && (curY >= minY) && (curY <= maxY) =
    Just $ (grid A.! curX) A.! curY
  | otherwise = Nothing
  where (minX, maxX) = A.bounds grid
        (minY, maxY) = A.bounds $ grid A.! minX

parseCell :: Char -> Cell
parseCell '#' = Bug
parseCell '.' = Empty
parseCell _   = error "Unrecognized character"

parseInput :: [String] -> Grid
parseInput = toArray . map (toArray . map parseCell)

updateGrid :: Grid -> Grid
updateGrid curGrid = A.listArray (minX, maxX) $ map genRow [minX..maxX]
  where (minX, maxX) = A.bounds curGrid
        (minY, maxY) = A.bounds $ curGrid A.! minX

        genRow curX = A.listArray (minY, maxY) $ map (genCell curX) [minY..maxY]

        genCell curX curY =
          let neighborLocs = [(curX-1, curY),
                              (curX+1, curY),
                              (curX, curY-1),
                              (curX, curY+1)]
              neighborCells = neighborLocs >>= (maybeToList . cellAtLoc curGrid)
              numBugNeighbors = sum $ map (fromEnum . (== Bug)) neighborCells in
            case (cellAtLoc curGrid (curX, curY)) of
              Just Empty
                | (numBugNeighbors == 1) || (numBugNeighbors == 2) -> Bug
                | otherwise                                        -> Empty
              Just Bug
                | numBugNeighbors == 1 -> Bug
                | otherwise            -> Empty
                
              Nothing -> error $
                           "Could not read the given location: " ++
                             show (curX, curY)

findFirstRepeat :: Grid -> Grid
findFirstRepeat givenGrid =
  findFirstRepeat' S.empty givenGrid
  where findFirstRepeat' visitedGrids curGrid
          | S.member curGrid visitedGrids = curGrid
          | otherwise = findFirstRepeat' (S.insert curGrid visitedGrids)
                                         (updateGrid curGrid)

biodiversityRating :: Grid -> Int
biodiversityRating grid = sum $ map findScoreOfRow $ A.assocs grid
  where findScoreOfRow (curX, curRow) =
          sum $ map (findScoreOfCell curX) $ A.assocs curRow
          
        findScoreOfCell curX (curY, Bug) = 2 ^ (5*curX+curY)
        findScoreOfCell _    (_, Empty)  = 0

main :: IO ()
main = print $ biodiversityRating $ findFirstRepeat $ parseInput input
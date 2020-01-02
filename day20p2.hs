import Data.Char
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.PriorityQueue.FingerTree as P
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

toArray :: [a] -> A.Array Int a
toArray lst = A.listArray (0, length lst-1) lst

data Cell = Wall | Empty | InPortal String | OutPortal String deriving (Show, Eq, Ord)
type Location = (Int, Int)
type Grid = A.Array Int (A.Array Int Cell)

entryAtLoc :: A.Array Int (A.Array Int a) -> Location -> a
entryAtLoc twoDArray (x, y) = (twoDArray A.! x) A.! y

createGrid :: A.Array Int (A.Array Int Char) -> Grid
createGrid inputArray =
  A.listArray (minX, maxX) $ map createRow [minX..maxX]
  where (minX, maxX) = A.bounds inputArray
        (minY, maxY) = A.bounds $ inputArray A.! minX

        createRow x = A.listArray (minY, maxY) $ map (createCell x) [minY..maxY]
        
        createCell x y
          | (x < minX+2) || (x > maxX-2) || (y < minY+2) || (y > maxY-2) = Wall
          | otherwise =
            let curCellIsEmpty = entryAtLoc inputArray (x, y) == '.'

                left1 = entryAtLoc inputArray (x-1, y)
                left2 = entryAtLoc inputArray (x-2, y)
                leftName = left2:left1:""
                
                right1 = entryAtLoc inputArray (x+1, y)
                right2 = entryAtLoc inputArray (x+2, y)
                rightName = right1:right2:""
                
                up1 = entryAtLoc inputArray (x, y-1)
                up2 = entryAtLoc inputArray (x, y-2)
                upName = up2:up1:""
                
                down1 = entryAtLoc inputArray (x, y+1)
                down2 = entryAtLoc inputArray (x, y+2)
                downName = down1:down2:""

                portalFunc =
                  if (x == minX+2) || (x == maxX-2) || (y == minY+2) || (y == maxY-2)
                    then InPortal
                    else OutPortal in
            if curCellIsEmpty
              then case () of
                     ()
                       | (isLetter left1) && (isLetter left2) ->
                         portalFunc leftName
                       | (isLetter right1) && (isLetter right2) ->
                         portalFunc rightName
                       | (isLetter up1) && (isLetter up2) ->
                         portalFunc upName
                       | (isLetter down1) && (isLetter down2) ->
                         portalFunc downName
                       | otherwise -> Empty
              else Wall

type PortalMap = M.Map Cell Location

findPortals :: Grid -> PortalMap
findPortals grid = foldr updateMap M.empty allLocs
  where (minX, maxX) = A.bounds grid
        (minY, maxY) = A.bounds $ grid A.! minX
        allLocs = (,) <$> [minX..maxX] <*> [minY..maxY]

        updateMap curLoc curMap =
          case (entryAtLoc grid curLoc) of
            curCell@(InPortal _)  -> M.insert curCell curLoc curMap
            curCell@(OutPortal _) -> M.insert curCell curLoc curMap
            _                     -> curMap

findBadPortals :: PortalMap -> [Cell]
findBadPortals = findBadPortals' . M.keys
  where findBadPortals' = filter =<< (not `compose2` oppositePortalExists)
  
        oppositePortalExists portalLst (InPortal name) =
          OutPortal name `elem` portalLst
        oppositePortalExists portalLst (OutPortal name) =
          InPortal name `elem` portalLst
        oppositePortalExists _ _ = error "Cell is not a portal"

data State = State Int Location deriving (Show, Eq, Ord)

calcAnswer :: Grid -> PortalMap -> Maybe Int
calcAnswer grid portalMap = calcAnswer' S.empty $
                              P.singleton (0, 0) (Empty, initialLoc)
  where initialLoc = fromJust $ M.lookup (InPortal "AA") portalMap
        
        calcAnswer' visitedStates statesDeque =
          case (P.minViewWithKey statesDeque) of
            Nothing -> Nothing
            
            Just (((curLevel, curSteps), (curCell, curLoc@(curX, curY))), rstQueue)
              | S.member (State curLevel curLoc) visitedStates ->
                calcAnswer' visitedStates rstQueue
              | otherwise ->
                traceShow (curLevel, curSteps, curCell, curLoc) $
                let newVisited = S.insert (State curLevel curLoc) visitedStates in
                case curCell of
                  InPortal "ZZ"
                    | curLevel == 0 -> Just curSteps
                    | otherwise     -> calcAnswer' newVisited rstQueue
                  InPortal "AA"
                    | curLevel == 0 -> error "Returned to initial state"
                    | otherwise     -> calcAnswer' newVisited rstQueue
                  
                  InPortal name
                    | curLevel == 0 -> calcAnswer' newVisited rstQueue
                    | otherwise ->
                      let otherPortal = OutPortal name
                          newLoc = fromJust $ M.lookup otherPortal portalMap
                          newQueue =
                            P.add (curLevel-1, curSteps+1)
                                  (Empty, newLoc)
                                  rstQueue in
                        calcAnswer' newVisited newQueue
                  OutPortal name ->
                    let otherPortal = InPortal name
                        newLoc = fromJust $ M.lookup otherPortal portalMap
                        newQueue =
                          P.add (curLevel+1, curSteps+1)
                                (Empty, newLoc)
                                rstQueue in
                      calcAnswer' newVisited newQueue
                  
                  Empty ->
                    let newLocs = [(curX-1, curY),
                                   (curX+1, curY),
                                   (curX, curY-1),
                                   (curX, curY+1)]
                        validLocs =
                          filter ((/= Wall) . entryAtLoc grid) newLocs
                        newAssocs = do
                          loc <- validLocs
                          return ((curLevel, curSteps+1),
                                  (entryAtLoc grid loc, loc))
                        
                        newQueue = foldr (uncurry P.add) rstQueue newAssocs in
                      calcAnswer' newVisited newQueue
                        
                  Wall -> error "Reached wall"

main :: IO ()
main = interact ((++"\n") . show . (flip calcAnswer =<< findPortals) . createGrid . toArray . map toArray . lines)
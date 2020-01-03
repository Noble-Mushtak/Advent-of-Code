import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.List as L

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

rawProgram :: [Int]
rawProgram = [3,8,1005,8,325,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,29,1006,0,41,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,54,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,102,1,8,76,1,9,11,10,2,5,2,10,2,1107,19,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,110,2,1007,10,10,2,1103,13,10,1006,0,34,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,142,1006,0,32,1,101,0,10,2,9,5,10,1006,0,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,179,1,1005,11,10,2,1108,11,10,1006,0,10,1,1004,3,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,216,1,1002,12,10,2,1102,3,10,1,1007,4,10,2,101,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,253,2,104,3,10,1006,0,70,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,282,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,305,101,1,9,9,1007,9,962,10,1005,10,15,99,109,647,104,0,104,1,21102,838211572492,1,1,21102,342,1,0,1105,1,446,21102,825326674840,1,1,21101,0,353,0,1106,0,446,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,29086686211,1,21102,1,400,0,1106,0,446,21102,209420786919,1,1,21101,0,411,0,1105,1,446,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,838337298792,1,21101,434,0,0,1105,1,446,21101,988661154660,0,1,21102,1,445,0,1106,0,446,99,109,2,21201,-1,0,1,21101,40,0,2,21101,0,477,3,21101,0,467,0,1105,1,510,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,472,473,488,4,0,1001,472,1,472,108,4,472,10,1006,10,504,1101,0,0,472,109,-2,2106,0,0,0,109,4,1201,-1,0,509,1207,-3,0,10,1006,10,527,21102,0,1,-3,22102,1,-3,1,22102,1,-2,2,21101,0,1,3,21101,546,0,0,1105,1,551,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,574,2207,-4,-2,10,1006,10,574,21201,-4,0,-4,1105,1,642,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,593,0,1105,1,551,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,612,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,634,21202,-1,1,1,21102,1,634,0,105,1,509,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]

type Program = A.Array Int Int

program :: Program
program = A.listArray (0, 1200) (rawProgram ++ (repeat 0))

data Mode = Position | Immediate | Relative deriving (Enum)

runProgram' :: [Int] -> Int -> Int -> Program -> [Int]
runProgram' inputs relBase curLoc prog =
  let infoCell = prog A.! curLoc
      opCode = infoCell `mod` 100
      mode1 = toEnum $ (infoCell `quot` 100) `mod` 10 
      mode2 = toEnum $ (infoCell `quot` 1000) `mod` 10
      mode3 = toEnum $ (infoCell `quot` 10000) `mod` 10 in
  case opCode of
    99 -> []
    _ -> let (_, lastInd) = A.bounds prog
             lastParamLoc = curLoc+(numParams opCode) in
         if lastParamLoc <= lastInd then
           let runP = runProgram' inputs relBase in
           case opCode of
             1 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (+)
             2 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (*)
             
             3 -> case inputs of
                    [] -> error "Not enough inputs"
                    input:rst ->
                      runProgram' rst relBase (curLoc+2) $
                         prog A.// [((readLoc mode1 (curLoc+1)), input)]
                           
             4 -> (:) (readElem mode1 (curLoc+1)) $
                    runP (curLoc+2) prog
                    
             5 -> if ((readElem mode1 (curLoc+1)) == 0)
                  then runP (curLoc+3) prog
                  else runP (readElem mode2 (curLoc+2)) prog
             6 -> if ((readElem mode1 (curLoc+1)) /= 0)
                  then runP (curLoc+3) prog
                  else runP (readElem mode2 (curLoc+2)) prog
                  
             7 -> runP (curLoc+4) $
                    calcOp mode1 mode2 mode3 (fromEnum `compose2` (<))
             8 -> runP (curLoc+4) $
                    calcOp mode1 mode2 mode3 (fromEnum `compose2` (==))

             9 -> runProgram' inputs (relBase+(readElem mode1 (curLoc+1))) (curLoc+2) prog
                    
             _ -> error "Unrecognized opcode"
         else error "Not enough parameters"
  where numParams 1 = 3
        numParams 2 = 3
        numParams 3 = 1
        numParams 4 = 1
        numParams 5 = 2
        numParams 6 = 2
        numParams 7 = 3
        numParams 8 = 3
        numParams 9 = 1
        numParams _ = 0

        readLoc Position loc = prog A.! loc
        readLoc Immediate loc = loc
        readLoc Relative loc = relBase+(prog A.! loc)

        readElem m loc = prog A.! (readLoc m loc)

        calcOp mode1 mode2 mode3 f =
          let firstElem = readElem mode1 (curLoc+1)
              secondElem = readElem mode2 (curLoc+2)
              destLoc = readLoc mode3 (curLoc+3) in
          prog A.// [(destLoc, f firstElem secondElem)]

runProgram :: Program -> [Int] -> [Int]
runProgram prog inputs = runProgram' inputs 0 0 prog

curryList :: [a] -> [(a, a)]
curryList (x:y:rst) = (x, y) : curryList rst
curryList _       = []

data Direction = RIGHT | UP | LEFT | DOWN deriving (Enum)

turnLeft :: Direction -> Direction
turnLeft DOWN = RIGHT
turnLeft dir  = succ dir

turnRight :: Direction -> Direction
turnRight RIGHT = DOWN
turnRight dir   = pred dir

type Point = (Int, Int)

updatePoint :: Direction -> Point -> Point
updatePoint RIGHT (x, y) = (x+1, y)
updatePoint UP    (x, y) = (x, y+1)
updatePoint LEFT  (x, y) = (x-1, y)
updatePoint DOWN  (x, y) = (x, y-1)

type Hull = (Direction, Point, M.Map Point Int)

colorUnderPanel :: Hull -> Int
colorUnderPanel (_, curLoc, curMap) = M.findWithDefault 0 curLoc curMap

updateHull :: Hull -> (Int, Int) -> Hull
updateHull (curDir, curLoc, curMap) (color, direction) =
  let newMap = M.insert curLoc color curMap
      directionUpdate = if (direction == 0) then turnLeft else turnRight
      newDirection = directionUpdate curDir
      newLocation = updatePoint newDirection curLoc in
  (newDirection, newLocation, newMap)

finalHull :: Hull
finalHull =
  let allHulls = L.scanl' updateHull (UP, (0, 0), M.singleton (0, 0) 1) $
                    curryList outputs
      inputs = map colorUnderPanel allHulls
      outputs = runProgram program inputs
  in last allHulls

drawHull :: Hull -> [String]
drawHull (_, _, curMap) =
  map drawRow [maxY,(maxY-1)..minY]
  where panels = M.assocs curMap
        allXs = map (fst . fst) panels
        allYs = map (snd . fst) panels
        minX = minimum allXs
        minY = minimum allYs
        maxX = maximum allXs
        maxY = maximum allYs
        
        drawRow curY = map (drawPanel curY) [minX..maxX]
  
        drawPanel curY curX =
          case (M.findWithDefault 0 (curX, curY) curMap) of
            1 -> '#'
            0 -> '.'
            _ -> error "Unknown panel color"

main :: IO ()
main = putStrLn $ L.intercalate "\n" $ drawHull finalHull
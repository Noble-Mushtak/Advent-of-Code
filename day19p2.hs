import qualified Data.Array as A
--import qualified Data.List as L
import Control.Monad

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

rawProgram :: [Int]
rawProgram = [109,424,203,1,21101,0,11,0,1106,0,282,21102,1,18,0,1106,0,259,2101,0,1,221,203,1,21101,0,31,0,1106,0,282,21101,0,38,0,1106,0,259,21001,23,0,2,21202,1,1,3,21102,1,1,1,21102,1,57,0,1106,0,303,2102,1,1,222,20102,1,221,3,21001,221,0,2,21102,1,259,1,21102,80,1,0,1106,0,225,21102,106,1,2,21102,91,1,0,1105,1,303,1201,1,0,223,21001,222,0,4,21101,259,0,3,21102,1,225,2,21101,225,0,1,21101,0,118,0,1106,0,225,20101,0,222,3,21102,42,1,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,0,148,0,1106,0,259,1201,1,0,223,21001,221,0,4,20101,0,222,3,21101,10,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,195,0,0,106,0,108,20207,1,223,2,20102,1,23,1,21101,-1,0,3,21101,214,0,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1202,-4,1,249,22102,1,-3,1,22101,0,-2,2,21202,-1,1,3,21101,250,0,0,1105,1,225,21202,1,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22102,1,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21202,-2,1,3,21101,343,0,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22101,0,-4,1,21102,384,1,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,22102,1,1,-4,109,-5,2105,1,0]

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

parseOutput :: [Int] -> Bool
parseOutput [1] = True
parseOutput [0] = False
parseOutput _   = error "Unrecognized output"

getSightBounds :: Int -> (Int, Int)
getSightBounds y = getSightBounds' Nothing 0
  where getSightBounds' Nothing curX
          | parseOutput $ runProgram program [curX, y] =
            getSightBounds' (Just curX) (curX+1)
          | otherwise =
            getSightBounds' Nothing (curX+1)
        
        getSightBounds' (Just startX) curX
          | parseOutput $ runProgram program [curX, y] =
            getSightBounds' (Just startX) (curX+1)
          | otherwise = (startX, curX-1)

getRectangleBounds :: Int -> IO ()
getRectangleBounds minY = do
  let minSightBounds@(_, maxX) = getSightBounds minY
  putStrLn $ show minY ++ ": " ++ show minSightBounds
  let maxY = minY+99
      maxSightBounds@(minX, _) = getSightBounds maxY
  putStrLn $ show maxY ++ ": " ++ show maxSightBounds
  let difference = maxX-minX
  putStrLn $ "Result: " ++ show difference
  putStrLn $ if difference >= 99 then "VALID" else "INVALID"

main :: IO ()
main = do
  allInput <- getContents
  let inputLines = lines allInput
  forM_ (map read inputLines) getRectangleBounds

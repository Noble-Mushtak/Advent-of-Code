import qualified Data.Array as A
import qualified Control.Monad.Writer.Lazy as W
import Data.Monoid

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

rawProgram :: [Int]
rawProgram = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1001,1034,0,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,102,1,1035,1040,101,0,1038,1043,1001,1037,0,1042,1106,0,124,1001,1034,1,1039,1008,1036,0,1041,101,0,1035,1040,102,1,1038,1043,1001,1037,0,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,39,1032,1006,1032,165,1008,1040,3,1032,1006,1032,165,1102,2,1,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1101,1,0,1044,1105,1,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,73,1044,1106,0,224,1102,0,1,1044,1105,1,224,1006,1044,247,101,0,1039,1034,1001,1040,0,1035,102,1,1041,1036,1002,1043,1,1038,102,1,1042,1037,4,1044,1105,1,0,20,63,98,68,77,63,97,8,75,63,86,35,88,55,91,67,88,55,71,51,80,32,34,15,4,65,96,55,96,49,84,51,89,46,10,91,39,72,4,71,88,84,45,30,8,82,65,30,81,17,94,53,56,75,10,65,83,99,23,86,97,15,96,79,84,25,4,87,29,96,51,43,89,97,16,95,93,86,81,28,91,55,93,75,30,82,68,74,19,96,13,82,91,37,77,65,93,50,31,81,99,19,30,96,69,92,95,65,54,86,95,49,84,68,82,49,21,22,20,90,80,10,51,74,12,90,61,84,87,85,2,91,36,33,91,90,48,83,47,46,99,88,64,78,8,66,26,88,53,81,89,45,62,30,87,36,53,78,2,66,96,97,27,32,93,35,90,70,91,58,4,87,75,45,78,28,5,77,97,58,88,72,10,94,36,78,21,54,91,20,76,87,88,39,93,22,18,77,87,70,70,89,81,32,10,25,97,14,72,89,86,43,37,77,11,81,82,90,49,31,93,56,77,84,81,30,92,32,98,38,41,11,38,78,78,1,28,91,1,97,81,57,74,79,76,50,79,29,97,32,90,88,31,29,82,64,97,39,97,92,40,95,8,29,88,79,96,14,81,92,33,28,24,30,87,61,94,83,72,82,69,97,10,41,85,6,66,85,65,14,82,90,29,77,36,20,72,96,42,81,85,94,46,75,10,54,26,42,83,99,95,47,39,81,92,70,69,86,89,48,38,79,75,7,96,92,26,75,92,26,86,10,17,80,63,90,19,37,15,77,63,54,2,88,92,97,98,47,66,85,67,56,82,72,35,90,81,26,17,1,82,23,28,67,99,5,3,83,2,54,93,53,87,92,64,96,89,91,40,93,77,94,37,89,27,95,38,89,93,7,60,45,97,82,63,66,77,87,16,95,55,99,97,54,77,75,34,65,86,76,85,76,40,71,77,68,49,92,59,64,64,76,45,57,91,40,98,68,42,87,41,93,46,85,91,38,75,29,98,96,58,81,30,78,76,78,40,75,31,21,64,84,65,92,76,76,15,81,77,62,84,65,77,72,29,90,97,65,64,15,98,31,77,30,44,92,97,9,82,1,33,95,1,27,88,3,18,76,84,51,85,90,45,38,98,42,53,95,86,68,10,98,28,87,58,46,99,2,77,21,85,1,35,98,80,77,20,19,92,88,60,86,85,40,75,22,99,72,33,99,11,84,90,42,75,86,38,71,81,87,72,78,93,83,45,20,8,94,14,93,83,64,99,47,9,96,55,8,87,44,84,49,20,85,58,88,3,86,69,57,99,23,83,51,78,93,88,18,85,41,85,72,79,24,64,28,79,51,5,84,17,78,18,27,67,95,90,32,58,8,75,83,98,67,96,35,74,65,9,85,69,94,39,74,7,85,51,67,13,84,30,98,93,40,46,88,97,89,46,61,63,83,81,98,63,69,80,90,95,40,30,83,93,42,85,96,34,83,85,31,30,11,98,5,2,86,92,41,78,31,37,96,41,57,98,66,39,75,81,83,26,10,84,88,61,85,15,36,98,24,72,74,33,94,88,79,69,89,59,97,80,66,42,81,74,99,61,34,96,93,23,95,6,85,79,8,36,80,27,92,17,63,98,92,14,20,98,96,74,47,81,31,59,98,19,57,82,23,88,28,92,87,56,64,67,74,85,68,85,15,39,87,32,88,61,91,8,77,97,97,14,86,62,79,42,98,3,75,9,97,99,80,28,93,44,85,39,92,49,99,17,81,97,91,3,76,37,76,49,86,0,0,21,21,1,10,1,0,0,0,0,0,0]

type Program = A.Array Int Int

program :: Program
program = A.listArray (0, 3000) (rawProgram ++ (repeat 0))

data Mode = Position | Immediate | Relative deriving (Enum)

data ProgState = ProgState Int Int Program deriving (Show)

type ProgOutput = Last Int
type ProgWriter = W.Writer ProgOutput ProgState

contProgram :: [Int] -> ProgState -> ProgWriter
contProgram inputs state@(ProgState relBase curLoc prog) =
  let infoCell = prog A.! curLoc
      opCode = infoCell `mod` 100
      mode1 = toEnum $ (infoCell `quot` 100) `mod` 10 
      mode2 = toEnum $ (infoCell `quot` 1000) `mod` 10
      mode3 = toEnum $ (infoCell `quot` 10000) `mod` 10 in
  case opCode of
    99 -> return state
    _ -> let (_, lastInd) = A.bounds prog
             lastParamLoc = curLoc+(numParams opCode) in
         if lastParamLoc <= lastInd then
           let runP = (contProgram inputs) `compose2` (ProgState relBase) in
           case opCode of
             1 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (+)
             2 -> runP (curLoc+4) $ calcOp mode1 mode2 mode3 (*)
             
             3 -> case inputs of
                    [] -> return state
                    input:rst ->
                      contProgram rst $ ProgState relBase (curLoc+2) $
                         prog A.// [((readLoc mode1 (curLoc+1)), input)]
                           
             4 -> (W.tell $ Last $ Just $ readElem mode1 (curLoc+1)) >>
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

             9 -> contProgram inputs $
                    ProgState (relBase+(readElem mode1 (curLoc+1)))
                              (curLoc+2)
                              prog
                    
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

startProgram :: Program -> [Int] -> ProgWriter
startProgram prog inputs = contProgram inputs $ ProgState 0 0 prog

oppositeDir :: Int -> Int
oppositeDir 1 = 2
oppositeDir 2 = 1
oppositeDir 3 = 4
oppositeDir 4 = 3
oppositeDir d = error $ "The integer " ++ show d ++ " is not a direction"

type Path = [Int]
type PathState = ([Int], ProgWriter)

initialState :: PathState
initialState = ([], startProgram program [])

findAllPathsToOxygen :: PathState -> [Path]
findAllPathsToOxygen (path, curWriter) =
  [1,2,3,4] >>= findAllPathsAlongDirection
  where findAllPathsAlongDirection curDir =
          let lastDir = case path of
                          []       -> Nothing
                          lstDir:_ -> Just lstDir in
          if lastDir == (Just $ oppositeDir curDir)
          then []
          else let newPath   = curDir : path
                   newWriter = curWriter >>= contProgram [curDir] in
               case (getLast $ W.execWriter newWriter) of
                 Just 0 -> []
                 Just 1 -> findAllPathsToOxygen (newPath, newWriter)
                 Just 2 -> [newPath]
                 out    -> error $ "Unrecognized output: " ++ show out

main :: IO ()
main = print $ minimum $ map length $ findAllPathsToOxygen initialState
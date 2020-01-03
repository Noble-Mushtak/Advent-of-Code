import qualified Data.Array as A

rawProgram :: [Int]
rawProgram = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0]
-- rawProgram = [1,9,10,3,2,3,11,0,99,30,40,50]

program :: A.Array Int Int
program = A.listArray (0, (length rawProgram)-1) rawProgram
modifiedProgram :: A.Array Int Int
modifiedProgram = program A.// [(1, 12), (2, 2)]

runProgram :: A.Array Int Int -> Int -> Maybe (A.Array Int Int)
runProgram prog curLoc =
  let opCode = prog A.! curLoc in
  case opCode of
    99 -> Just prog
    _ -> let (_, lastInd) = A.bounds prog in
         if (curLoc+4) <= lastInd then
           let firstLoc = prog A.! (curLoc+1)
               secondLoc = prog A.! (curLoc+2)
               firstElem = prog A.! firstLoc
               secondElem = prog A.! secondLoc
               destLoc = prog A.! (curLoc+3) in
           case opCode of
             1 -> runProgram (prog A.// [(destLoc, firstElem+secondElem)]) (curLoc+4)
             2 -> runProgram (prog A.// [(destLoc, firstElem*secondElem)]) (curLoc+4)
             _ -> Nothing
         else Nothing

main :: IO ()
main = let result = runProgram modifiedProgram 0 in
  case result of
    Nothing -> print "ERROR: Program could not be run to completion"
    Just prog -> print $ prog A.! 0
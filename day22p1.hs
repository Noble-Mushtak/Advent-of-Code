import qualified Data.Array as A
import qualified Data.List as L
--import Debug.Trace

calcModuloInverse :: Int -> Int -> Int
calcModuloInverse num1 num2 = calcModuloInverse' (1, 0, num2) (0, 1, num1)
  where calcModuloInverse' _ (_, y, 1) = y
        calcModuloInverse' (x1, y1, n1) (x2, y2, n2) =
          let multiplier = n1 `quot` n2
              newX = x1-(multiplier*x2)
              newY = y1-(multiplier*y2)
              newN = n1-(multiplier*n2) in
            calcModuloInverse' (x2, y2, n2) (newX, newY, newN)

parseLine :: Int -> [String] -> [Int]
parseLine numCards ["deal", "into", "new", "stack"] =
  --trace "deal into new stack" $
  reverse [0..numCards-1]
  
parseLine numCards ["deal", "with", "increment", numStr] =
  --trace ("deal with increment " ++ numStr) $
  let num = read numStr
      numInverse = calcModuloInverse num numCards in
    map ((`mod` numCards) . (numInverse *)) [0..numCards-1]
    
parseLine numCards ["cut", numStr] =
  --trace ("cut " ++ numStr) $ 
  let num = read numStr
      trueNum = if num < 0 then num+numCards else num
      (firstNElems, restLst) = splitAt trueNum [0..numCards-1] in
    restLst ++ firstNElems
    
parseLine unknownWords _ = error $ "Unrecognized input: " ++ show unknownWords

inverseArray :: [Int] -> A.Array Int Int
inverseArray lst = A.array (0, length lst-1) $
                     zip lst [0..length lst-1]

calcAnswer :: Int -> Int -> String -> Int
calcAnswer numCards desiredCard input =
  let linesOfInput = lines input
      inverseFuncsFromInput =
        map (inverseArray . (parseLine numCards) . words) linesOfInput
      desiredPos = L.foldl' (flip (A.!)) desiredCard inverseFuncsFromInput in
    desiredPos

main :: IO ()
main = interact ((++"\n") . show . (calcAnswer 10007 2019))
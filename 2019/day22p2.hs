calcModuloInverse :: Integer -> Integer -> Integer
calcModuloInverse num1 num2 = calcModuloInverse' (1, 0, num2) (0, 1, num1)
  where calcModuloInverse' _ (_, y, 1) = y
        calcModuloInverse' (x1, y1, n1) (x2, y2, n2) =
          let multiplier = n1 `quot` n2
              newX = x1-(multiplier*x2)
              newY = y1-(multiplier*y2)
              newN = n1-(multiplier*n2) in
            calcModuloInverse' (x2, y2, n2) (newX, newY, newN)

data LinearFunction = LinearFunction Integer Integer Integer deriving (Show)

parseLine :: Integer -> [String] -> LinearFunction
parseLine numCards ["deal", "into", "new", "stack"] =
  LinearFunction numCards (-1) (-1)
  
parseLine numCards ["deal", "with", "increment", numStr] =
  let num = read numStr
      numInverse = calcModuloInverse num numCards in
    LinearFunction numCards numInverse 0
    
parseLine numCards ["cut", numStr] =
  --trace ("cut " ++ numStr) $ 
  let num = read numStr in
    LinearFunction numCards 1 num
    
parseLine unknownWords _ = error $ "Unrecognized input: " ++ show unknownWords

calcFunction :: Integer -> String -> LinearFunction
calcFunction numCards input =
  let linesOfInput = lines input
      funcsFromInput = map ((parseLine numCards) . words) linesOfInput in
    foldr1 composeLinearFuncs funcsFromInput

--m1*(m2*x+b2)+b1=(m1*m2)*x+(m1*b2+b1)
composeLinearFuncs :: LinearFunction -> LinearFunction -> LinearFunction
composeLinearFuncs (LinearFunction mod1 m1 b1)
                   (LinearFunction mod2 m2 b2)
  | mod1 == mod2 = LinearFunction mod1
                                  ((m1*m2) `mod` mod1)
                                  ((m1*b2+b1) `mod` mod1)
  | otherwise = error "Unequal modulos in linear functions"

iterateLinearFunc :: Integer -> LinearFunction -> LinearFunction
iterateLinearFunc numTimes linearFunc@(LinearFunction mod1 _ _)
  | numTimes == 0 = LinearFunction mod1 1 0
  | numTimes == 1 = linearFunc
  | (numTimes `mod` 2) == 0 =
    let subFunc = iterateLinearFunc (numTimes `quot` 2) linearFunc in
      subFunc `composeLinearFuncs` subFunc
  | otherwise = 
    let subFunc = iterateLinearFunc (numTimes `quot` 2) linearFunc in
      subFunc `composeLinearFuncs` subFunc `composeLinearFuncs` linearFunc

applyLinearFunc :: LinearFunction -> Integer -> Integer
applyLinearFunc (LinearFunction mod1 m1 b1) input =
  (m1*input+b1) `mod` mod1

main :: IO ()
main = interact ((++"\n") . show . (`applyLinearFunc` 2020) . (iterateLinearFunc 101741582076661) . (calcFunction 119315717514047))
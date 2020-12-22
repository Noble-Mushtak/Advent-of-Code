inputSignal :: [Int]
inputSignal = [5,9,7,6,8,0,9,2,8,3,9,9,2,7,7,5,8,5,6,5,1,9,1,2,9,8,6,2,5,2,1,5,1,0,6,3,7,1,8,9,0,1,1,8,0,5,1,4,2,6,2,5,0,8,5,5,9,2,4,7,6,4,1,9,4,4,1,1,5,2,8,0,0,4,7,1,8,7,0,9,8,8,6,4,0,2,9,0,3,4,3,5,5,6,9,6,2,7,9,8,2,4,8,5,3,0,1,9,2,1,6,4,9,2,4,0,8,2,0,0,5,9,8,2,7,1,6,1,0,2,4,6,3,1,6,1,2,2,9,0,0,0,5,1,0,6,3,0,4,7,2,4,8,4,6,6,8,0,4,1,5,6,9,0,1,8,3,3,7,1,4,6,9,0,3,7,4,1,8,1,2,6,3,8,3,4,5,0,3,7,0,7,4,1,0,7,8,6,8,4,9,7,4,5,9,8,6,6,2,6,4,2,9,5,6,7,9,4,0,1,2,8,2,5,2,7,1,4,8,7,3,2,9,2,4,3,5,8,3,1,1,7,5,3,7,8,7,3,5,6,5,3,3,2,1,6,6,7,4,4,1,2,8,8,4,5,0,0,6,8,0,6,8,7,8,7,1,7,9,5,5,9,4,6,5,3,4,1,5,8,8,3,7,3,7,0,4,5,1,9,3,5,9,1,9,7,9,0,4,6,9,8,1,5,1,4,3,3,4,1,5,9,9,8,2,0,0,1,6,4,6,9,3,6,8,6,8,4,8,9,3,1,2,2,7,6,6,8,5,7,2,6,1,4,2,6,7,9,9,6,3,6,5,5,9,5,2,5,0,0,3,8,7,7,0,9,0,5,7,9,8,4,5,7,2,5,6,7,6,4,8,1,2,7,6,9,7,7,7,8,1,2,7,0,6,2,7,5,5,8,9,0,1,4,3,3,5,0,1,5,6,5,3,3,7,4,0,9,7,1,6,8,5,8,9,4,9,2,0,3,4,3,0,1,8,1,1,0,3,2,7,8,1,9,4,4,2,8,5,4,6,3,8,5,0,6,3,9,1,1,2,3,9,4,7,8,8,0,4,7,1,7,7,4,4,9,7,7,9,9,8,8,4,1,4,3,4,0,6,1,6,8,8,0,0,0,3,8,3,4,5,6,1,7,6,4,9,4,2,1,0,6,9,1,8,6,1,9,5,7,2,4,3,3,7,0,2,4,5,1,7,0,2,2,3,8,6,2,3,0,4,6,6,3,9,3,2,8,7,4,4,5,4,6,2,4,2,3,4,2,2,6,3,6,1,6,4,2,6,7,8,2,5,9,0,2,0,0,9,4,8,0,1,7,7,4,8,2,5,6,9,4,4,2,3,0,6,0,7,0,0,3,1,2,5,0,4,2,8,6,4,7,5,3,0,5,6,7,4,8,6,4,4,4,2,2,5,0,7,0,9,0,2,9,8,1,2,3,7,9]

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

calcNthPattern :: Int -> [Int]
calcNthPattern n = tail $ concat $ repeat $ concat $ map (replicate n) basePattern

calcFFT :: [Int] -> [Int]
calcFFT curSignal = map (applyPattern . calcNthPattern) [1..length curSignal]
  where applyPattern = (`mod` 10) . abs . sum . zipWith (*) curSignal

main :: IO ()
main = print $ take 8 $ iterate calcFFT inputSignal !! 100
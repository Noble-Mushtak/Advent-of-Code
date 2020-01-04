import Data.List.Split

layerLength :: Int
layerLength = 25 * 6

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

getInfo :: String -> (Int, Int, Int)
getInfo = (,,) <$> countElem '0' <*> countElem '1' <*> countElem '2'

calcAnswer :: (Int, Int, Int) -> Int
calcAnswer (_, num1s, num2s) = num1s*num2s

main :: IO ()
main = interact ((++"\n") . show . calcAnswer . minimum . map getInfo . chunksOf layerLength)
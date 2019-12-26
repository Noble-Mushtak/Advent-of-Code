import Data.List.Split

layerLength :: Int
layerLength = 25 * 6

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

getInfo :: String -> (Int, Int, Int)
getInfo = (,,) <$> countElem '0' <*> countElem '1' <*> countElem '2'

main :: IO ()
main = interact (show . minimum . map getInfo . chunksOf layerLength)
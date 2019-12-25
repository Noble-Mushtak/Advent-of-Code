import qualified Data.Map.Strict as M
import qualified Data.Set as S

youOrbit :: String
youOrbit = "VW9"

sanOrbit :: String
sanOrbit = "Y81"

parseInput :: String -> [(String, String)]
parseInput = (map (sanitize . (break (==')')))) . lines
  where sanitize (a, b) = (a, tail b)

buildMap :: [(String, String)] -> M.Map String (S.Set String)
buildMap = foldr addNewEdge M.empty
  where addNewEdge (start, end) = M.alter (addVertex start) end . M.alter (addVertex end) start
  
        addVertex vert Nothing = Just $ S.singleton vert
        addVertex vert (Just vertsSoFar) = Just $ S.insert vert vertsSoFar

calcDistance :: M.Map String (S.Set String) -> S.Set String -> String -> String -> Int
calcDistance edgeList visited end start
  | S.member start visited = 1000000
  | start == end           = 0
  | otherwise              = let newVisited = S.insert start visited
                                 calcDistanceToEnd = calcDistance edgeList newVisited end in
                             S.foldr min 1000000 $
                               S.map ((+1) . calcDistanceToEnd) $
                                 M.findWithDefault S.empty start edgeList

main :: IO ()
main = interact (show . (\m -> calcDistance m S.empty sanOrbit youOrbit) . buildMap . parseInput)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

parseInput :: String -> [(String, String)]
parseInput = (map (sanitize . (break (==')')))) . lines
  where sanitize (a, b) = (a, tail b)

buildMap :: [(String, String)] -> M.Map String (S.Set String)
buildMap = foldr addNewEdge M.empty
  where addNewEdge (start, end) = M.alter (addVertex end) start
  
        addVertex vert Nothing = Just $ S.singleton vert
        addVertex vert (Just vertsSoFar) = Just $ S.insert vert vertsSoFar

calcAllReachables :: M.Map String (S.Set String) -> M.Map String Int
calcAllReachables edgeList = M.map calcReachable edgeList
  where calcReachable neighbors =
          sum $
            map (\v -> 1+(calcReachable (M.findWithDefault S.empty v edgeList))) $
              S.elems neighbors

main :: IO ()
main = interact ((++"\n") . show . M.foldr (+) 0 . calcAllReachables . buildMap . parseInput)
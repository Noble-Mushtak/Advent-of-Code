import qualified Data.Set as S

type Point = (Int, Int)

allLocsBetween :: Point -> Point -> S.Set Point
allLocsBetween (x1, y1) (x2, y2) =
  let numSpaces = gcd (x1-x2) (y1-y2)
      diffX = (x1-x2) `quot` numSpaces
      diffY = (y1-y2) `quot` numSpaces
      genPoint i = (x2+i*diffX, y2+i*diffY) in
  S.fromList $ map genPoint [1..(numSpaces-1)]

labelChars :: Int -> String -> [(Point, Char)]
labelChars yCoord row = zipWith labelChar [0..] row
  where labelChar xCoord ch = ((xCoord, yCoord), ch)

calcLocations :: [String] -> S.Set Point
calcLocations = S.fromList .
                  concat .
                    map (map fst . filter ((=='#') . snd)) .
                      zipWith labelChars [0..]

numCanBeSeen :: S.Set Point -> Point -> Int
numCanBeSeen allAsteroids station =
  let noAsteroidsBetween =
        S.null . S.intersection allAsteroids . allLocsBetween station
      seeable = (&&) <$> ((/=) station) <*> noAsteroidsBetween in
  S.size $ S.filter seeable allAsteroids

maxNumCanBeSeen :: S.Set Point -> Int
maxNumCanBeSeen allAsteroids =
  S.foldr (max . (numCanBeSeen allAsteroids)) 0 allAsteroids

main :: IO ()
main = interact $ (show . maxNumCanBeSeen . calcLocations . lines)
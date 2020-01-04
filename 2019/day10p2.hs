import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.List as L

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 f g a b = f $ g a b

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

maxNumCanBeSeen :: S.Set Point -> (Int, Point)
maxNumCanBeSeen allAsteroids =
  let labelPoint = (,) =<< (numCanBeSeen allAsteroids) in
  S.foldr (max . labelPoint) (0, (-1, -1)) allAsteroids

compareAngles :: Point -> Point -> Point -> Ordering
compareAngles (stX, stY) (x1, y1) (x2, y2)
  | (xd1 >= 0) && (xd2 < 0) = LT
  | (xd1 < 0) && (xd2 >= 0) = GT
  -- yd1/xd1 ?? yd2/xd2 -> yd1*xd2 ?? yd2*xd1
  | otherwise               = (yd1*xd2) `compare` (yd2*xd1)
  where (xd1, yd1) = (x1-stX, y1-stY)
        (xd2, yd2) = (x2-stX, y2-stY)

calcVaporizeOrder :: A.Array Int [Point] -> Int -> [Point]
calcVaporizeOrder asteroidsLeft curInd =
  case (asteroidsLeft A.! curInd) of
    [] -> calcVaporizeOrder asteroidsLeft nextInd
    nextAsteroid:rst -> let newAsteroids = asteroidsLeft A.// [(curInd, rst)] in
                        nextAsteroid : calcVaporizeOrder newAsteroids nextInd
  where numAsteroids = 1+(snd $ A.bounds asteroidsLeft)
        nextInd = (curInd+1) `mod` numAsteroids

calcAnswer :: String -> String
calcAnswer input =
  let asteroids = calcLocations $ lines input
      (_, station) = maxNumCanBeSeen asteroids
      asteroidsLst = filter ((/=) station) $ S.elems asteroids
      sortedAsteroids = L.sortBy (compareAngles station) asteroidsLst
      onALine = (==EQ) `compose2` compareAngles station
      groupedAsteroids = L.groupBy onALine sortedAsteroids
      groupedAsteroidsArr = A.listArray (0, (length groupedAsteroids)-1) groupedAsteroids
      vaporizationOrder = calcVaporizeOrder groupedAsteroidsArr 0 in
  show $ vaporizationOrder !! 199

main :: IO ()
main = interact ((++"\n") . calcAnswer)
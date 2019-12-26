import qualified Data.Map.Strict as M

type Point = (Int, Int, Int)

initialMoonLocations :: [Point]
initialMoonLocations = [(14, 4, 5), (12, 10, 8), (1, 7, -10), (16, -5, 3)]

data Moon = Moon Point Point deriving (Show, Eq)

initialMoons :: [Moon]
initialMoons = zipWith Moon initialMoonLocations $ repeat (0, 0, 0)

updateVelocity :: [Moon] -> Moon -> Moon
updateVelocity otherMoons (Moon curPos@(cx, cy, cz) curVel) = Moon curPos newVel
  where newVel = foldr updateVelocity' curVel otherMoons
  
        updateVelocity' (Moon (mx, my, mz) _) (vx, vy, vz)
         = (updateVelocityCoord mx cx vx,
            updateVelocityCoord my cy vy,
            updateVelocityCoord mz cz vz)
        
        updateVelocityCoord moonCoord curCoord velCoord =
          case (moonCoord `compare` curCoord) of
            LT -> velCoord-1
            EQ -> velCoord
            GT -> velCoord+1

updatePosition :: Moon -> Moon
updatePosition (Moon (x, y, z) (vx, vy, vz)) =
  Moon (x+vx, y+vy, z+vz) (vx, vy, vz)

updateMoons :: [Moon] -> [Moon]
updateMoons moons = map (updatePosition . updateVelocityWithOtherMoons) moons
  where updateVelocityWithOtherMoons =
          (=<<) updateVelocity $ flip filter moons . (/=)

data OneDMoon = OneDMoon Int Int deriving (Eq, Ord, Show)

getMoonX :: Moon -> OneDMoon
getMoonX (Moon (cx, _, _) (vx, _, _)) = OneDMoon cx vx

getMoonY :: Moon -> OneDMoon
getMoonY (Moon (_, cy, _) (_, vy, _)) = OneDMoon cy vy

getMoonZ :: Moon -> OneDMoon
getMoonZ (Moon (_, _, cz) (_, _, vz)) = OneDMoon cz vz

findCycle' :: Int -> M.Map [OneDMoon] Int -> (Moon -> OneDMoon) -> [Moon] -> (Int, Int)
findCycle' stepsSoFar curMap getCoord moons =
  case (M.lookup moonCoords curMap) of
    Nothing -> findCycle' (stepsSoFar+1) newMap getCoord (updateMoons moons)
    Just steps -> (steps, stepsSoFar-steps)
  where moonCoords = map getCoord moons
        newMap = M.insert moonCoords stepsSoFar curMap

findCycle :: (Moon -> OneDMoon) -> [Moon] -> (Int, Int)
findCycle = findCycle' 0 M.empty

main :: IO ()
main =
   let xAns@(xStart, xCycle) = findCycle getMoonX initialMoons
       yAns@(yStart, yCycle) = findCycle getMoonY initialMoons
       zAns@(zStart, zCycle) = findCycle getMoonZ initialMoons
       cycleStart = max xStart $ max yStart zStart
       cycleLength = lcm xCycle $ lcm yCycle zCycle in
   do print xAns
      print yAns
      print zAns
      print $ cycleStart+cycleLength
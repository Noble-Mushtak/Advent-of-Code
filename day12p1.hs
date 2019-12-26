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

calcPotentialEnergy :: Moon -> Int
calcPotentialEnergy (Moon (cx, cy, cz) _) = (abs cx)+(abs cy)+(abs cz)

calcKineticEnergy :: Moon -> Int
calcKineticEnergy (Moon _ (vx, vy, vz)) = (abs vx)+(abs vy)+(abs vz)

calcEnergy :: Moon -> Int
calcEnergy = (*) <$> calcPotentialEnergy <*> calcKineticEnergy

main :: IO ()
main = print $ sum $ map calcEnergy $ iterate updateMoons initialMoons !! 1000
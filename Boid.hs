-- All the constant came from trial and error, there might be better constants.
module Boid where
import Prelude
import Vector
import Data.List.PointedList
import Control.Comonad
import Data.List

-- Make PointedList a Comonad for that sweet sweet =>> syntax.
instance Comonad PointedList where
    duplicate = positions
    extract (PointedList _ p _) = p


data Boid = Boid {pos, vel, acc :: Vector , maxSpeed, maxForce :: Double}
    deriving (Show)

-- I think that this is redundant.
createBoid :: (Double, Double) -> (Double, Double) -> (Double, Double)
    -> Double -> Double -> Boid
createBoid p v a ms mf = Boid (toVec p) (toVec v) (toVec a) ms mf
    where
        toVec = uncurry Vector

-- Update takes a flock and updates the boid that is in focus right now
-- with respect to the other boids in the flock.
update :: (Double, Double) -> Flock -> Boid
update (x, y) f =
    let boid = extract f
        -- Get the acceleration and integrate twice to get the position.
        newAcc = flock f (Vector x y)
        newVel = (vel boid) + newAcc
        newVel' = limit (maxSpeed boid) newVel
        newPos = pos boid + newVel'
    in boid{pos = newPos, vel = newVel', acc = origin}

-- The "reach" for each boid, notice that the reach does not span across the
-- border i.e. a boid on the far left on the screen are not affected by a boid
-- on the far right.
radius :: Double
radius = 80.0

defaultBoid :: Boid
defaultBoid = Boid origin origin origin 2.0 0.01

type Flock = PointedList Boid

-- Add all the different flocking behaviours.
flock :: Flock -> Vector -> Vector
flock f mousePos =
    let sep = separate f * 2
        ali = align f
        coh = cohesion f * 0.05
        avo = avoid f mousePos
    in origin + ali + coh + sep + avo


getPos :: Boid -> (Double, Double)
getPos Boid{pos = p} = toTuple p

distance :: Boid -> Boid -> Double
distance b1 b2 = Vector.distance (pos b1) (pos b2)

-- Get the average of a list of vectors, if the list is empty, return 0.
average :: [Vector] -> Vector
average vs = sum vs / (toVector $ max (genericLength vs) 1)

separate :: Flock -> Vector
separate f =
    average $ map (\b ->
        normal (pos boid - pos b) / (toVector (Boid.distance b boid)))
             (insideRadius f (radius / 2))
        where boid = extract f

-- Return all boids of the flock wich are in the reach of the boid in focus.
insideRadius :: PointedList Boid -> Double -> [Boid]
insideRadius (PointedList ls boid rs) rad = filter (\b ->
        let d = Boid.distance b boid
        in d < rad && d /= 0) boids
    where
        boids = ls ++ rs

align :: Flock -> Vector
align f =
    limit 0.1 $ average $ map vel $ insideRadius f radius

cohesion :: Flock -> Vector
cohesion f =
    steer (extract f) $ average $ map pos $ insideRadius f radius

steer :: Boid -> Vector -> Vector
steer boid target =
    let desiredDir = normal $ target - pos boid
        desiredSpeed = desiredDir * (toVector (maxSpeed boid))
        steering = desiredSpeed - vel boid
    in  limit (maxForce boid) steering

-- There should be a little more avoidance of the mouse than of other
-- boids.
avoid :: Flock -> Vector -> Vector
avoid f enemyPos =
    if dist < radius * 2 then
        4 * normal (pos boid - enemyPos) / (toVector $ dist)
    else origin
    where
        boid = extract f
        dist = Vector.distance (pos boid) enemyPos


-- All the constant came from trial and error, there might be better constants.
module Boid where
import Prelude
import Linear.V2 (V2(V2))
import Linear.Metric (distance, norm, normalize, signorm)
import Linear.Vector
import Data.List.PointedList
import Control.Comonad
import Data.List

origin :: V2 Double
origin = V2 0 0

limit :: Double -> V2 Double -> V2 Double
limit m v
    | norm v > m = m *^ normalize v
    | otherwise = v

-- Make PointedList a Comonad for that sweet sweet =>> syntax.
instance Comonad PointedList where
    duplicate = positions
    extract (PointedList _ p _) = p

data Boid = Boid {pos, vel, acc :: V2 Double , maxSpeed, maxForce :: Double}
    deriving (Show, Eq)

type Flock = PointedList Boid

-- Update takes a flock and updates the boid that is in focus right now
-- with respect to the other boids in the flock.
update :: V2 Double -> Flock -> Boid
update mpos f =
    let boid = extract f
        -- Get the acceleration and integrate twice to get the position.
        newAcc = flock f mpos
        newVel = vel boid + newAcc
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

-- Add all the different flocking behaviours.
flock :: Flock -> V2 Double -> V2 Double
flock f mpos =
    let sep = separate f * 2
        ali = align f
        coh = cohesion f * 0.05
        avo = avoid f mpos
    in ali + coh + sep + avo

distance :: Boid -> Boid -> Double
distance b1 b2 = Linear.Metric.distance (pos b1) (pos b2)

-- Get the average of a list of V2s, if the list is empty, return 0.
average :: [V2 Double] -> V2 Double
average vs = sum vs ^/ max (genericLength vs) 1

separate :: Flock -> V2 Double
separate f =
    average $ map (\b ->
        normalize (pos boid - pos b) ^/ Boid.distance b boid)
             (insideRadius f (radius / 2))
        where boid = extract f

-- Return all boids of the flock wich are in the reach of the boid in focus.
insideRadius :: PointedList Boid -> Double -> [Boid]
insideRadius (PointedList ls boid rs) rad = filter (\b ->
        let d = Boid.distance b boid
        in d < rad && d /= 0) boids
    where
        boids = ls ++ rs

align :: Flock -> V2 Double
align f =
    limit 0.1 $ average $ map vel $ insideRadius f radius

cohesion :: Flock -> V2 Double
cohesion f =
    steer (extract f) $ average $ map pos $ insideRadius f radius

steer :: Boid -> V2 Double -> V2 Double
steer boid target =
    let desiredDir = normalize $ target - pos boid
        desiredSpeed = desiredDir ^* maxSpeed boid
        steering = desiredSpeed - vel boid
    in  limit (maxForce boid) steering

-- There should be a little more avoidance of the mouse than of other
-- boids.
avoid :: Flock -> V2 Double -> V2 Double
avoid f enemyPos =
    if dist < radius * 2 then
        4 * normalize (pos boid - enemyPos) ^/ dist
    else origin
    where
        boid = extract f
        dist = Linear.Metric.distance (pos boid) enemyPos

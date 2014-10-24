-- Rape the num instance to get nice code elsewhere.
-- e.g (Vector 1 4) * 2 == Vector 2 8.
module Vector where

-- Why are the doubles strict? I have no good reason for that.
data Vector = Vector !Double !Double deriving (Eq)

instance Show Vector where
    show (Vector x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

vectorApply :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
vectorApply f (Vector x1 y1) (Vector x2 y2) = Vector (x1 `f` x2) (y1 `f` y2)
instance Num Vector where
    (+) = vectorApply (+)
    (-) = vectorApply (-)
    (*) = vectorApply (*)
    signum = undefined
    abs (Vector x y) = Vector (abs x) (abs y)
    fromInteger a = Vector (fromInteger a) (fromInteger a)

instance Fractional Vector where
    (/) = vectorApply (/)
    fromRational a = Vector (fromRational a) (fromRational a)

-- dot product
(<.>) :: Vector -> Vector -> Double
(Vector x1 y1) <.> (Vector x2 y2) = x1 * x2 + y1 * y2

distance :: Vector -> Vector -> Double
distance (Vector x1 y1) (Vector x2 y2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

magnitude :: Vector -> Double
magnitude = distance origin

origin :: Vector
origin = Vector 0 0

-- Return a Vector of length 1 in direction of v.
normal :: Vector -> Vector
normal v@(Vector 0 0) = v
normal v = v / (toVector (magnitude v))

toVector :: Double -> Vector
toVector a = Vector a a

toTuple :: Vector -> (Double, Double)
toTuple (Vector x y) = (x, y)

getX :: Vector -> Double
getX (Vector x _ ) = x
getY :: Vector -> Double
getY (Vector _ y) = y

limit :: Double -> Vector -> Vector
limit m v
    | magnitude v > m = normal v * toVector m
    | otherwise = v

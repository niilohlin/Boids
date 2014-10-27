module Test where
import Control.Comonad
import Test.QuickCheck
import Main
import Vector
import Boid
import Data.List.PointedList


instance Arbitrary Vector where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Vector x y

instance Arbitrary Boid where
    arbitrary = do
        pos' <- arbitrary
        vel' <- arbitrary
        acc' <- arbitrary
        maxSpeed' <- choose (1, 3)
        maxForce' <- choose (0.001, 0.1)
        return $ Boid pos' vel' acc' maxSpeed' maxForce'

instance Arbitrary a => Arbitrary (PointedList a) where
    arbitrary = do
        ls <- arbitrary
        p  <- arbitrary
        rs <- arbitrary
        return $ PointedList ls p rs

-- Every boid updates to a new position.
prop_update :: (Double, Double) -> Flock -> Bool
prop_update dim f = update dim f /= extract f

-- Really just quickchecking math here.
prop_positive_distance  :: Vector -> Vector -> Bool
prop_positive_distance v u = Vector.distance v u >= 0

prop_limit_border :: (Positive Int, Positive Int) -> Flock -> Bool
prop_limit_border ((Positive w), (Positive h)) f = all (\b ->
    let v = pos b
        x = getX v
        y = getY v
        w' = fromIntegral w
        h' = fromIntegral h
    in (-w' / 2) <= x && x <= (w' / 2) &&
       (-h' / 2) <= y && y <= (h' / 2)) $ toList (limitToBorder (w, h) f)



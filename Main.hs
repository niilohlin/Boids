-- This is the view of the program.
module Main where
import FRP.Helm
import FRP.Elerea.Simple
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Mouse  as Mouse
import Control.Comonad
import Data.List.PointedList
import Boid
import Vector
-- mod' is rational mod
import Data.Fixed (mod')


toList :: PointedList Boid -> [Boid]
toList (PointedList ls p rs) = ls ++ [p] ++ rs

-- The engine is needed to get the signal for the dimensions of the window.
-- The reason for the lifting it to "toList" is because the draw method
-- should not know anything about the underlying datastructure.
flockSignal :: Engine -> SignalGen (Signal [Boid])
flockSignal engine = toList <~ do
    dim' <- Window.dimensions engine
    pos' <- Mouse.position
    transfer2 initState nextState dim' pos'
        where
            -- The fancy =>> operator is the Comonad "extend" operator. Check it out. It's awesome.
            -- This reads "For all Boids in flock, limit them to border, then update everyone
            -- with respect to the mouse.
            nextState dim mpos flock' = limitToBorder dim flock' =>> (update (mouseOrtho dim mpos))
            -- mouseOrtho exist because helm is weird and mixes origin on the middle of the screen
            -- for drawing and in the upper left corner for the mouse.
            mouseOrtho (w, h) (x, y) = (fromIntegral x - fromIntegral w / 2,
                                        fromIntegral y - fromIntegral h / 2)
            initState = PointedList [] defaultBoid [
                -- Two boids are mathematically identical if they have the same
                -- initial position. I did not feel like using random data here.
                Boid (Vector 2 0) 1 0 2 0.01
               ,Boid (Vector 3 1) 1 0 2 0.01
               ,Boid (Vector 4 0) 1 0 2 0.01
               ,Boid (Vector 1 4) 1 0 2 0.01
               ,Boid (Vector 1 1) 1 0 2 0.01
               ,Boid (Vector 1 0) 1 0 2 0.01
               ,Boid (Vector 0 1) 1 0 2 0.01
               ,Boid (Vector (-3) 1) 1 0 2 0.01
               ,Boid (Vector (-4) 0) 1 0 2 0.01
               ,Boid (Vector (-1) 4) 1 0 2 0.01
               ,Boid (Vector (-1) 1) 1 0 2 0.01
               ,Boid (Vector (-1) 0) 1 0 2 0.01
               ,Boid (Vector (-0) 1) 1 0 2 0.01
               ,Boid (Vector 3 (-1)) 1 0 2 0.01
               ,Boid (Vector 4 (-7)) 1 0 2 0.01
               ,Boid (Vector 1 (-4)) 1 0 2 0.01
               ,Boid (Vector 1 (-1)) 1 0 2 0.01
               ,Boid (Vector 1 (-5)) 1 0 2 0.01
               ,Boid (Vector 0 (-1)) 1 0 2 0.01
               ,Boid (Vector (-2) (-2)) 0 0 2 0.01]

-- Limit to border makes everything stay on the screen.
-- If a boid goes on to the left, it continues on from the right.
limitToBorder :: (Int, Int) -> Flock -> Flock
limitToBorder (w, h) = fmap (\boid -> boid{
    pos = Vector ((getX $ pos boid) `borderMod` fromIntegral w)
                 ((getY $ pos boid) `borderMod` fromIntegral h)
                 })
    where
        borderMod a b = (a + (b / 2)) `mod'` b - b / 2

drawBoid :: Boid -> Form
drawBoid boid = rotate (angle + correctDir) $ move (getPos boid) $ filled red $
                                    polygon $ path [(-5, 0), (0, 20), (5, 0)]
    where
        angle = atan . getTan $ vel boid
        getTan (Vector x y) = if x == 0 then y else y / x
        -- correctDir subtracts pi / 2 when x of vel > 0
        -- and add pi / 2 otherwise. It is because -pi / 2 < atan x < pi / 2
        -- for all x.
        correctDir = signum (getX (vel boid)) * pi / 2

render :: (Int, Int) -> [Boid] -> Element
render (w, h) boids =
    centeredCollage w h $ map drawBoid boids

main :: IO ()
main = do
    engine <- startup defaultConfig {windowDimensions = (800, 600)}
    run engine $ render <~ Window.dimensions engine ~~ flockSignal engine


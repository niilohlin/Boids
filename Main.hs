-- This is the view of the program.
module Main where
import Helm
import qualified Helm.Mouse as Mouse
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Cmd as Cmd
import Helm.Graphics2D
import Helm.Color
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time)
import Control.Comonad
import Data.List.PointedList
import Boid
import Linear.V2 (V2(V2))
-- mod' is rational mod
import Data.Fixed (mod')

windowDims :: V2 Int
windowDims = V2 800 600

data Action
   = Animate
   | MoveTo (V2 Double)

data Model = Model { flock :: Flock, mpos :: V2 Double }

getX :: V2 a -> a
getX (V2 x _) = x

getY :: V2 a -> a
getY (V2 _ y) = y

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update (Model flock _) (MoveTo newPos) = (Model flock newPos, Cmd.none)
update (Model flock mpos) Animate = (Model (limitToBorder windowDims flock =>> Boid.update mpos) mpos, Cmd.none)

-- Limit to border makes everything stay on the screen.
-- If a boid goes on to the left, it continues on from the right.
limitToBorder :: V2 Int -> Flock -> Flock
limitToBorder (V2 w h) = fmap (\boid -> boid {
    pos = V2 (getX (pos boid) `mod'` fromIntegral w)
             (getY (pos boid) `mod'` fromIntegral h)
                 })

drawBoid :: Boid -> Form SDLEngine
drawBoid boid = rotate (angle + correctDir) $ move (pos boid) $ filled (rgb 1 0 0) $
                                    polygon $ path [V2 (-5) 0, V2 0 20, V2 5 0]
    where
        angle = atan . getTan $ vel boid
        getTan (V2 x y) = if x == 0 then y else y / x
        -- correctDir subtracts pi / 2 when x of vel > 0
        -- and add pi / 2 otherwise. It is because -pi / 2 < atan x < pi / 2
        -- for all x.
        correctDir = -signum (getX (vel boid)) * pi / 2

toList :: PointedList Boid -> [Boid]
toList (PointedList ls p rs) = ls ++ [p] ++ rs

view :: Model -> Graphics SDLEngine
view (Model flock _) = Graphics2D $ collage $ map drawBoid $ toList flock

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
                [
                  Mouse.moves $ fmap MoveTo (fromIntegral <$>)
                , Time.fps 60 $ const Animate
                ]

initial :: (Model, Cmd SDLEngine Action)
initial =
  (Model (PointedList [] defaultBoid [
          -- Two boids are mathematically identical if they have the same
          -- initial position. I did not feel like using random data here.
           Boid (V2 2 0) 1 0 2 0.01
         , Boid (V2 3 1) 1 0 2 0.01
         , Boid (V2 4 0) 1 0 2 0.01
         , Boid (V2 1 4) 1 0 2 0.01
         , Boid (V2 1 1) 1 0 2 0.01
         , Boid (V2 1 0) 1 0 2 0.01
         , Boid (V2 0 1) 1 0 2 0.01
         , Boid (V2 (-3) 1) 1 0 2 0.01
         , Boid (V2 (-4) 0) 1 0 2 0.01
         , Boid (V2 (-1) 4) 1 0 2 0.01
         , Boid (V2 (-1) 1) 1 0 2 0.01
         , Boid (V2 (-1) 0) 1 0 2 0.01
         , Boid (V2 (-0) 1) 1 0 2 0.01
         , Boid (V2 3 (-1)) 1 0 2 0.01
         , Boid (V2 4 (-7)) 1 0 2 0.01
         , Boid (V2 1 (-4)) 1 0 2 0.01
         , Boid (V2 1 (-1)) 1 0 2 0.01
         , Boid (V2 1 (-5)) 1 0 2 0.01
         , Boid (V2 0 (-1)) 1 0 2 0.01
         , Boid (V2 (-3) (-1)) 1 0 2 0.01
         , Boid (V2 (-4) (-7)) 1 0 2 0.01
         , Boid (V2 (-1) (-4)) 1 0 2 0.01
         , Boid (V2 (-1) (-1)) 1 0 2 0.01
         , Boid (V2 (-1) (-5)) 1 0 2 0.01
         , Boid (V2 (-0) (-1)) 1 0 2 0.01
         , Boid (V2 (-2) (-2)) 0 0 2 0.01])
         $ pure 100
    , Cmd.none
  )

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine defaultConfig GameLifecycle
    { initialFn       = initial
    , updateFn        = Main.update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

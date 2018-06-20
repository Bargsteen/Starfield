module Main where

import Linear.V2 (V2(V2))
import System.Random

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Time as Time
import qualified Helm.Engine.SDL as SDL

data Action = Idle | Tick
newtype Star = Star (V2 Double)
newtype Model = Model [Star]
newtype SpeedFactor = SpeedFactor Int


initial :: [Star] -> (Model, Cmd SDLEngine Action)
initial stars = (Model stars, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model stars) Tick = ( Model $ map (moveAwayFrom wCenter (SpeedFactor 2)) stars, Cmd.none)
  where wCenter = V2 0 0 -- Center in translated coordinate system

subscriptions :: Sub SDLEngine Action
subscriptions = Time.every Time.millisecond $ const Tick

view :: Model -> Graphics SDLEngine
view (Model stars) = Graphics2D $ collage $ fmap (\(Star pos) -> move (translateToCenter pos) $ filled (rgb 1 1 1) $ circle 5) stars
  where
    halfWSize = windowSize / 2
    translateToCenter = translate (V2 halfWSize halfWSize)


main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig

  g <- newStdGen
  let randomVals = randomRs (-halfWSize, halfWSize) g
  let stars = zipWith (\x y -> Star (V2 x y)) (take 100 randomVals) ((take 100 . drop 100) randomVals)

  run engine GameConfig
    { initialFn       = initial stars
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
  where halfWSize = windowSize / 2


windowSize :: Double
windowSize = 800

engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.SDLEngineConfig (V2 wSize wSize) False False "Starfield"
  where wSize = round windowSize

translate :: V2 Double -> V2 Double -> V2 Double
translate a b = a + b

moveAwayFrom :: V2 Double -> SpeedFactor -> Star -> Star
moveAwayFrom from (SpeedFactor sf) (Star pos@(V2 x y)) = Star newPos
  where
    hws = windowSize / 2
    newPos = pos + vMult (pos - from) 0.01


vPow :: (Num a, Integral b) => V2 a -> b -> V2 a
vPow (V2 x y) f = V2 (x ^ f) (y ^ f)

vMult :: (Num a) => V2 a -> a -> V2 a
vMult (V2 x y) m = V2 (x*m) (y*m)

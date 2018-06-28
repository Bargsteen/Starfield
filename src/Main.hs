module Main where

import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time       as Time

import qualified System.Random   as Rand

type Chance = Double
data Action = Idle | Tick | MaybeNewStar Chance | NewStar Star
data Star = Star {prevPos :: V2 Double, pos :: V2 Double, vel :: V2 Double}
newtype Model = Model [Star]

initial :: (Model, Cmd SDLEngine Action)
initial = (Model [], Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model stars) Tick = (Model $ filter starOutOfSight $ map (applyVelocity . addForceAwayFromCenter 0.01) stars, Cmd.execute genChance MaybeNewStar)
update model (MaybeNewStar chance) = if chance >= 0.3 then (model, Cmd.execute genStar NewStar) else (model, Cmd.none)
update (Model stars) (NewStar newStar) = (Model (newStar:stars), Cmd.none)


subscriptions :: Sub SDLEngine Action
subscriptions = Time.every Time.millisecond $ const Tick

view :: Model -> Graphics SDLEngine
view (Model stars) = Graphics2D $ collage $ fmap (\star -> move (translateToCenter (pos star)) $ filled (rgb 1 1 1) $ circle (size star)) stars
  where
    translateToCenter = translate (V2 halfWSize halfWSize)
    distFromCenterToCorner = mag $ (V2 halfWSize halfWSize) - screenCenter
    size star = min 10 $ normalizeToRange (0, distFromCenterToCorner) (0.1, 6) $ mag $ pos star - screenCenter
    screenCenter = V2 0 0


main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

type LowerLimit = Double
type UpperLimit = Double

genChance :: IO Chance
genChance = head <$> genRandomDoubles (0,1)

genRandomDoubles :: (LowerLimit, UpperLimit) -> IO [Double]
genRandomDoubles (l, u) = Rand.randomRs (l, u) <$> Rand.newStdGen

genStar :: IO Star
genStar = do
  randomDoubles <- genRandomDoubles (-halfWSize, halfWSize)
  let (x, y) = (head randomDoubles, randomDoubles !! 1)
  return (Star (V2 x y) (V2 x y) (V2 0 0))


windowSize :: Double
windowSize = 800

halfWSize :: Double
halfWSize = windowSize / 2

engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.SDLEngineConfig (V2 wSize wSize) False False "Starfield"
  where wSize = round windowSize

translate :: V2 Double -> V2 Double -> V2 Double
translate a b = a + b


type Factor = Double

starOutOfSight :: Star -> Bool
starOutOfSight star = x >= lowerBuffer && x <= upperBuffer && y >= lowerBuffer && y <=  upperBuffer
  where
    (V2 x y) = pos star
    lowerBuffer = (-halfWSize) - 20
    upperBuffer = halfWSize + 20


addForceAwayFromCenter :: Factor -> Star -> Star
addForceAwayFromCenter factor star = addForce awayForce star
  where
    from = V2 0 0
    awayForce = vMult factor $ pos star - from

addForce :: V2 Double -> Star -> Star
addForce f s = Star (prevPos s) (pos s) vel'
  where
    vel' = vel s + f

applyVelocity :: Star -> Star
applyVelocity s = Star prevPos' pos' vel'
  where
    prevPos' = pos s
    vel' = vel s
    pos' = prevPos' + vel'

vMult :: (Num a) => a -> V2 a -> V2 a
vMult m (V2 x y) = V2 (x*m) (y*m)

mag :: (Floating a) => V2 a -> a
mag (V2 x y) = sqrt $ (x^2) + (y^2)


type Value = Double
type FromBottom = Double
type FromTop = Double
type ToBottom = Double
type ToTop = Double

normalizeToRange :: (FromBottom, FromTop) -> (ToBottom, ToTop) -> Value -> Value
normalizeToRange (fb, ft) (tb, tt) val = tb + (tt - tb) * ((val - fb) / (ft - fb))

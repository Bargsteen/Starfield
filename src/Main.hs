module Main where

import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Sub        as Sub
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Time       as Time
import qualified Helm.Mouse      as Mouse

import qualified System.Random   as Rand

type Count = Int
data Action = Idle | Tick | MaybeNewStars Count | NewStars [Star] | MouseMoved (V2 Int)
data Star = Star {prevPos :: V2 Double, pos :: V2 Double, vel :: V2 Double}
type Speed = Double
data Model = Model [Star] Speed

initial :: (Model, Cmd SDLEngine Action)
initial = (Model [] 0.01, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model stars speed) Tick = (Model (filter starOutOfSight $ map (applyVelocity . addForceAwayFromCenter speed) stars) speed, Cmd.execute (genCount 0 50) MaybeNewStars)
update model (MaybeNewStars count) = (model, Cmd.execute (genStars count) NewStars)
update (Model stars speed) (NewStars newStars) = (Model (newStars ++ stars) speed, Cmd.none)
update (Model stars _) (MouseMoved (V2 x _)) = (Model stars $ normalizeToRange (0, windowSize) (0.0000001, 0.05) (fromIntegral x), Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                          , Mouse.moves MouseMoved
                          ]

view :: Model -> Graphics SDLEngine
view (Model stars _) = Graphics2D $ collage [background, lineForms]
  where
    translateToCenter = translate (V2 halfWSize halfWSize)
    distFromCenterToCorner = mag $ V2 halfWSize halfWSize - screenCenter
    size star = min 10 $ normalizeToRange (0, distFromCenterToCorner) (0.1, 6) $ mag $ pos star - screenCenter
    screenCenter = V2 0 0
    white = rgb 1 1 1
    darkGrey = rgb 0.1 0.1 0.1
    red = rgb 1 0 0
    background = toForm $ collage $ pure $ move (translateToCenter screenCenter) $ filled darkGrey $ square windowSize
    oldStarForms = toForm $ collage $ fmap (\star -> move (translateToCenter $ prevPos star) $ filled red $ circle (size star)) stars
    starForms = toForm $ collage $ fmap (\star -> move (translateToCenter (pos star)) $ filled white $ circle (size star)) stars
    lineForms = toForm $ collage $ fmap (\star -> move (translateToCenter screenCenter) $ traced (solid white) $ Path [prevPos star, pos star]) stars

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
type MinCount = Int
type MaxCount = Int

genCount :: MinCount -> MaxCount -> IO Count
genCount minCount maxCount = head . Rand.randomRs (minCount, maxCount) <$> Rand.newStdGen


genRandomDoubles :: LowerLimit -> UpperLimit -> IO [Double]
genRandomDoubles l u = Rand.randomRs (l, u) <$> Rand.newStdGen

genStars :: Count -> IO [Star]
genStars count =
  take count . fmap (\(x, y) -> Star (V2 x y) (V2 x y) (V2 0 0)) . toPairs <$> genRandomDoubles (-halfWSize) halfWSize


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


toPairs :: [a] -> [(a,a)]
toPairs [] = []
toPairs [_] = []
toPairs (x:y:xs) = (x, y) : toPairs xs

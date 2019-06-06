module Main where

import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Mouse      as Mouse
import qualified Helm.Sub        as Sub
import qualified Helm.Time       as Time

import qualified System.Random   as Rand


-- ** TYPES ** --

data Model = Model [Star] Speed
data Action = Idle | Tick | GenStars UpToCount | AddStars [Star] | MouseMoved (V2 Int)
data Star = Star {prevPos :: V2 Double, pos :: V2 Double, vel :: V2 Double}

type Factor = Double
type UpToCount = Int
type Speed = Double

type LowerLimit = Double
type UpperLimit = Double
type MinCount = Int
type MaxCount = Int

type Value = Double
type FromBottom = Double
type FromTop = Double
type ToBottom = Double
type ToTop = Double


-- ** HELM ** --

initial :: (Model, Cmd SDLEngine Action)
initial = (Model [] 0.01, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model stars speed) Tick = (Model stars' speed, Cmd.execute (genCount 0 maxStarCount) GenStars)
  where
    stars' = filter starOutOfSight $ map (applyVelocity . addForceAwayFromCenter speed) stars
    maxStarCount = round $ 5000 * speed
update model (GenStars count) = (model, Cmd.execute (genStars count) AddStars)
update (Model stars speed) (AddStars newStars) = (Model (newStars ++ stars) speed, Cmd.none)
update (Model stars _) (MouseMoved (V2 x _)) = (Model stars $ normalizeToRange (0, windowSize) (minSpeed, maxSpeed) (fromIntegral x), Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [ Time.every Time.millisecond $ const Tick
                          , Mouse.moves MouseMoved
                          ]

view :: Model -> Graphics SDLEngine
view (Model stars _) = Graphics2D $ collage $ map (toForm . collage) [background, theStars, theLines]
  where
    translateToCenter = translate (V2 halfWSize halfWSize)
    distFromCenterToCorner = mag $ V2 halfWSize halfWSize - screenCenter
    size star = normalizeToRange (0, distFromCenterToCorner) (minStarSize, maxStarSize) $ mag $ pos star - screenCenter
    background = pure $ move (translateToCenter screenCenter) $ filled darkGrey $ square windowSize
    theStars = map (\star -> move (translateToCenter (pos star)) $ filled white $ circle (size star)) stars
    theLines = map (\star -> move (translateToCenter screenCenter) $ traced (solid white) $ Path [prevPos star, pos star]) stars

main :: IO ()
main = do
  engine <- SDL.startupWith engineConfig

  run engine defaultConfig GameLifecycle
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }


-- ** HELPER FUNCTIONS ** --

-- * Physics * --

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

starOutOfSight :: Star -> Bool
starOutOfSight star = x >= lowerBuffer && x <= upperBuffer && y >= lowerBuffer && y <=  upperBuffer
  where
    (V2 x y) = pos star
    lowerBuffer = (-halfWSize) - 20
    upperBuffer = halfWSize + 20


-- * Vectors * --

vMult :: (Num a) => a -> V2 a -> V2 a
vMult m (V2 x y) = V2 (x*m) (y*m)

mag :: (Floating a) => V2 a -> a
mag (V2 x y) = sqrt $ x^(2::Int) + y^(2::Int)

translate :: V2 Double -> V2 Double -> V2 Double
translate a b = a + b


-- * Random Generation * --

genCount :: MinCount -> MaxCount -> IO UpToCount
genCount minCount maxCount = head . Rand.randomRs (minCount, maxCount) <$> Rand.newStdGen

genRandomDoubles :: LowerLimit -> UpperLimit -> IO [Double]
genRandomDoubles l u = Rand.randomRs (l, u) <$> Rand.newStdGen

genStars :: UpToCount -> IO [Star]
genStars count =
  take count . fmap (\(x, y) -> Star (V2 x y) (V2 x y) (V2 0 0)) . toPairs <$> genRandomDoubles (-halfWSize) halfWSize


-- * Initialization and Conventient Constants * --

-- Engine
engineConfig :: SDL.SDLEngineConfig
engineConfig = SDL.SDLEngineConfig (V2 wSize wSize) False False "Starfield"
  where wSize = round windowSize

-- Screen
windowSize :: Double
windowSize = 800

halfWSize :: Double
halfWSize = windowSize / 2

screenCenter :: V2 Double
screenCenter = V2 0 0

-- Star sizes
minStarSize :: Double
minStarSize = 0.0000001

maxStarSize :: Double
maxStarSize = 3.5

-- Simulation Speed
minSpeed :: Double
minSpeed = 0.0000001

maxSpeed :: Double
maxSpeed = 0.03

-- Colors
white :: Color
white = rgb 1 1 1

darkGrey :: Color
darkGrey = rgb 0.1 0.1 0.1

-- * Conversion * --

normalizeToRange :: (FromBottom, FromTop) -> (ToBottom, ToTop) -> Value -> Value
normalizeToRange (fb, ft) (tb, tt) val = tb + (tt - tb) * ((val - fb) / (ft - fb))

toPairs :: [a] -> [(a,a)]
toPairs []       = []
toPairs [_]      = []
toPairs (x:y:xs) = (x, y) : toPairs xs

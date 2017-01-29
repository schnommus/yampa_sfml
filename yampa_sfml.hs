-- Title:    Yampa/SFML Stub

{-# LANGUAGE Arrows #-}

module Main where

import IdentityList

import Data.Maybe
import Data.List.Safe as ListSafe
import Control.Monad.Loops
import System.Random

import GHC.Float

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry
import FRP.Yampa.Random

import SFML.Graphics
import SFML.Window
import SFML.System

-- types --------

type Position2 = Vector2 Double
type Velocity2 = Vector2 Double
type Rotation = Float

type Input = [SFEvent]    -- non-deterministic events from input devices
type Logic = Yampa.Event () -- deterministic events from object processor

data ObjEvents = ObjEvents
    { oeInput :: Input
    , oeLogic :: Logic
    } deriving (Show)

data Renderable = RenderableSprite Sprite | RenderableText Text String

data State = Entity
    { entPosition :: Position2
    , entRenderable  :: Renderable
    , entRotation :: Rotation
    , entColor    :: Color
    }

instance Show State where
    show (Entity pos _ rotation color)
      = "Entity" ++ (show pos) ++ " " ++ (show rotation) ++ " " ++ (show color)

data ObjOutput = ObjOutput
    { ooState         :: State
    , ooKillRequest   :: Yampa.Event ()       -- NoEvent|Event ()
    , ooSpawnRequests :: Yampa.Event [Object]
    }

defaultObjOutput = ObjOutput
    { ooState         = undefined
    , ooKillRequest   = Yampa.NoEvent
    , ooSpawnRequests = Yampa.NoEvent
    }

type Object = SF ObjEvents ObjOutput


-- utility ---------

instance Show (SF a b) where
    show sf = "SF"


-- main ---------

centerSprite :: Sprite -> IO ()
centerSprite sprite = do
    bounds <- getLocalBounds sprite
    setOrigin sprite (Vec2f ((/2).fwidth$bounds) ((/2).fheight$bounds))

loadPixelSprite :: String -> IntRect -> IO Sprite
loadPixelSprite string rect = do
    sprite <- err $ createSprite
    tex <- err $ textureFromFile string Nothing
    setTexture sprite tex True
    setTextureRect sprite rect
    setScale sprite 3
    centerSprite sprite
    return sprite

playerObj :: IO Object
playerObj = do
    sprite1 <- loadPixelSprite "media/pixelship.png" (IntRect 0 0 32 32)
    sprite2 <- loadPixelSprite "media/pixelshipfire.png" (IntRect 0 0 32 32)
    bullet_spr <- loadPixelSprite "media/pixelbullet.png" (IntRect 0 0 3 3)
    smoke_spr <- loadPixelSprite "media/pixelstar.png" (IntRect 0 0 1 1)
    let gen = mkStdGen 0
    return $ playerObject (vector2 16 16) sprite1 sprite2 white bullet_spr smoke_spr gen

fpsObj :: IO Object
fpsObj = do
    text <- err $ createText
    font <- err $ fontFromFile "media/pixelfont.ttf"
    setTextFont text font
    setTextString text "???"
    return $ fpsObject (vector2 16 16) text white

init_window :: IO RenderWindow
init_window = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <-
        createRenderWindow
            (VideoMode 96 96 32)
            "Yampa/SFML Stub"
            [SFDefaultStyle]
            ctxSettings
    windowSize <- getWindowSize wnd
    putStrLn $ "Got window - size: " ++ (show $ windowSize)
    return wnd

main :: IO ()
main = do
    wnd <- init_window

    clock <- createClock
    player <- playerObj
    fps <- fpsObj

    reactimate (initialize wnd)
               (input wnd clock)
               (output wnd)
               (process (listToIL[player, fps]))


-- reactimation IO ----------

allPendingEvents :: RenderWindow -> IO Input
allPendingEvents wnd = do
    eventMaybe <- unfoldWhileM (/= Nothing) (pollEvent wnd)
    return $ (fromMaybe []) $ sequence eventMaybe


initialize :: RenderWindow -> IO Input
initialize wnd = do
    putStrLn "Initialize..."
    allPendingEvents wnd

input :: RenderWindow -> Clock -> Bool -> IO (DTime, Maybe Input)
input wnd clk _ = do
    events <- allPendingEvents wnd
    delta <- fmap (float2Double.asSeconds) (restartClock clk)
    return (delta, Just events)

yampaToSfVector :: Vector2 Double -> Vec2f
yampaToSfVector v = Vec2f (f$vector2X v) (f$vector2Y v)
    where f = double2Float

output :: RenderWindow -> Bool -> IL ObjOutput -> IO Bool
output wnd _ oos = do
    clearRenderWindow wnd black
    mapM_ (\oo -> render (ooState oo)) (elemsIL oos) -- render 'State'!
    display wnd
    return $ null $ keysIL oos
  where
    render :: State -> IO ()
    render (Entity pos (RenderableSprite sprite) rotation color) = do
        setPosition sprite (yampaToSfVector pos)
        setRotation sprite rotation
        setColor sprite color
        draw wnd sprite Nothing
        return ()
    render (Entity pos (RenderableText text string) rotation color) = do
        setPosition text (yampaToSfVector pos)
        setRotation text rotation
        setTextString text string
        setTextColor text color
        draw wnd text Nothing
        return ()

-- reactimate process ----------

process :: IL Object -> SF Input (IL ObjOutput)
process objs0 = proc input -> do
    rec
        -- 'process' stores the 'State's (note: rec) and
        -- passes them over to core
        oos <- core objs0 -< (input, oos)
    returnA -< oos

core :: IL Object -> SF (Input, IL ObjOutput) (IL ObjOutput)
core objs = dpSwitch route
                     objs
                     (arr killAndSpawn >>> notYet)
                     (\sfs' f -> core (f sfs'))

-- 1. process previous object 'State's (if any) and
--    generate logical events
-- 2. distribute input and logical events to the corresponding objects
route :: (Input, IL ObjOutput) -> IL sf -> IL (ObjEvents, sf)
route (input, oos) objs = mapIL routeAux objs
  where
    hs = hits (assocsIL (fmap ooState oos)) -- process all object 'State's
    routeAux (k, obj) = (ObjEvents
        { oeInput = input
        -- hit events are only routed to the objects they belong to (routing)
        , oeLogic = if k `elem` hs then Event () else Yampa.NoEvent
        }, obj)

hits :: [(ILKey, State)] -> [ILKey]
hits kooss = concat (hitsAux kooss)
  where
    hitsAux [] = []
    -- Check each object 'State' against each other
    hitsAux ((k,oos):kooss) =
        [ [k, k'] | (k', oos') <- kooss, oos `hit` oos' ]
        ++ hitsAux kooss

    hit :: State -> State -> Bool
    (Entity p1 _ _ _) `hit` (Entity p2 _ _ _) = p1 == p2

killAndSpawn :: ((Input, IL ObjOutput), IL ObjOutput)
             -> Yampa.Event (IL Object -> IL Object)
killAndSpawn ((input, _), oos) =
    if any checkEscKey input || any checkExit input
        then Event (\_ -> emptyIL) -- kill all 'State' on [Esc] => quit
        else foldl (mergeBy (.)) noEvent events
  where
    events :: [Yampa.Event (IL Object -> IL Object)]
    events = [ mergeBy (.)
                      (ooKillRequest oo `tag` (deleteIL k))
                      (fmap  (foldl (.) id . map insertIL_)
                             (ooSpawnRequests oo))
             | (k, oo) <- assocsIL oos ]
    checkEscKey (SFEvtKeyPressed KeyEscape _ _ _ _) = True
    checkEscKey _ = False
    checkExit SFEvtClosed = True
    checkExit _ = False


-- objects ----------

-- True if pressed, false if released, nothing otherwise
scanKey :: KeyCode -> Input -> Maybe Bool
scanKey key =
    ListSafe.last . (mapMaybe $ \evt -> case evt of
        (SFEvtKeyPressed k _ _ _ _) -> if k == key then Just True else Nothing
        (SFEvtKeyReleased k _ _ _ _) -> if k == key then Just False else Nothing
        _ -> Nothing)

-- Create a signal function that tracks the state of keys based on input events.
-- It emits 'true' whenever the specified keycode is pressed
trackKey :: KeyCode -> SF Input Bool
trackKey key = proc input -> do
    rec
        keyState <- iPre False -<
            case scanKey key input of
                Nothing -> keyState
                Just newState -> newState
    returnA -< keyState

-- Create a signal function that returns a value depending on a boolean input
applyValue :: a -> a -> SF Bool a
applyValue true_value false_value =
    arr $ \input -> case input of
      True -> true_value
      False -> false_value

radToDeg :: Float -> Float
radToDeg f = 180*(f/3.14159)

radToVec :: Double -> Velocity2
radToVec r = vector2 (cos r) (sin r)

playerObject :: RandomGen r =>
    Position2 -> Sprite -> Sprite -> Color -> Sprite -> Sprite -> r -> Object
playerObject p0 sprite_still sprite_move color bullet_spr smoke_spr gen = proc objEvents -> do
    left  <- applyValue (-1) 0 <<< trackKey KeyLeft  -< (oeInput objEvents)
    right <- applyValue  1   0 <<< trackKey KeyRight -< (oeInput objEvents)
    up    <- applyValue  1   0 <<< trackKey KeyUp    -< (oeInput objEvents)

    engines_on <- trackKey KeyUp -< (oeInput objEvents)

    sprite <- applyValue sprite_move sprite_still -< engines_on

    smokeSource <- occasionally gen (1/smoke_particles) () -< ()
    let createSmokeEvent = gate smokeSource engines_on


    rec
        rot_acc <- identity -< (rot_speed *) $ (left + right)
        pos_acc <- identity -<  ((up * pos_speed) *^) (radToVec rot)

        rot_vel <- integral -< rot_acc - (rot_drag * rot_vel)
        pos_vel <- integral -< pos_acc ^-^ (pos_drag *^ pos_vel)

        rot <- integral -< rot_vel
        p <- (p0^+^) ^<< integral -< pos_vel

    createBulletEvent <- edge <<< trackKey KeySpace -< (oeInput objEvents)

    smokeEngine <- ((*0.5).(`subtract`1).fromIntegral.round) ^<< noiseR (0 :: Float, 2) gen -< ()
    smokeOffset <- noiseR ((-0.1), 0.1) gen -< ()
    let smokePosition = p ^+^ (50 *^ (radToVec (rot+pi+smokeOffset+smokeEngine)))

    returnA -< defaultObjOutput {
        ooState = Entity
                    p
                    (RenderableSprite sprite)
                    ((90+).radToDeg.double2Float$rot)
                    color,
        ooSpawnRequests = catEvents $ [
            createBulletEvent `tag`
                particleObject bullet_spr p (proj_v pos_vel rot 1000),
            createSmokeEvent `tag`
                particleObject smoke_spr smokePosition (negateVector (proj_v pos_vel rot 100))
                                      ]
        }
        where pos_speed = 500
              pos_drag = 0.5
              rot_speed = 50
              rot_drag = 10
              smoke_particles = 160
              proj_v :: Velocity2 -> Double -> Double -> Velocity2
              proj_v vel rot factor = vel ^+^ (factor*^(radToVec rot))

particleObject :: Sprite -> Position2 -> Velocity2 -> Object
particleObject sprite pos vel = proc objEvents -> do
    pos_vel <- constant vel -< ()
    pos_out <- (pos^+^) ^<< integral -< pos_vel
    decay <- (+1.0) ^<< integral -< (-1.0)

    decayed_event <- edge <<^ (< 0.1) -< decay

    let fade :: Double -> Color
        fade a = Color 255 255 30 ((round.(*255)) a)

    returnA -< defaultObjOutput {
        ooState = Entity pos_out (RenderableSprite sprite) 0 (fade decay),
        ooKillRequest = decayed_event
                                }

fpsObject :: Vector2 Double -> Text -> Color -> Object
fpsObject pos text color = proc objEvents -> do
    rec
        elapsed <- localTime -< ()
        oldElapsed <- iPre 0.1 -< elapsed
        fps <- arr ((++" FPS").show.round.(1.0/)) -< elapsed - oldElapsed
    returnA -< defaultObjOutput {
        ooState = Entity pos (RenderableText text fps) 0 color
                                }

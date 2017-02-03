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

data State = Entity Position2 Renderable Rotation Color |
             Camera Position2

instance Show State where
    show (Entity pos _ rotation color)
      = "Entity " ++ (show pos) ++ " " ++ (show rotation) ++ " " ++ (show color)
    show (Camera pos)
      = "Camera " ++ (show pos)

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
    setScale sprite 1
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
    setTextCharacterSize text 8
    return $ fpsObject (vector2 4 4) text white

cameraObj :: IO Object
cameraObj = do
    return $ cameraObject (vector2 0 0)

data RenderSystem = RenderSystem
    { renderWindow :: RenderWindow
    , renderTexture :: RenderTexture
    , renderSprite :: Sprite
    }

init_rendersystem :: IO RenderSystem
init_rendersystem = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <-
        createRenderWindow
            (VideoMode 96 96 32)
            "Yampa/SFML Stub"
            [SFDefaultStyle]
            ctxSettings
    windowSize <- getWindowSize wnd
    putStrLn $ "Got window - size: " ++ (show $ windowSize)

    let vx, vy :: Vec2u -> Int
        vx (Vec2u x _) = fromIntegral x `div` 3
        vy (Vec2u _ y) = fromIntegral y `div` 3

    renderTex <- err $
        createRenderTexture (vx windowSize) (vy windowSize) False

    renderSpr <- err $ createSprite
    setScale renderSpr 3

    return $ RenderSystem wnd renderTex renderSpr

main :: IO ()
main = do
    renderSystem <- init_rendersystem

    clock <- createClock
    player <- playerObj
    fps <- fpsObj
    camera <- cameraObj

    reactimate (initialize $ renderWindow renderSystem)
               (input (renderWindow renderSystem) clock)
               (output renderSystem)
               (process (listToIL[player, fps, camera]))


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

yampaToSfVectorRounded :: Vector2 Double -> Vec2f
yampaToSfVectorRounded v = Vec2f (f$vector2X v) (f$vector2Y v)
    where f = fromIntegral.round

output :: RenderSystem -> Bool -> IL ObjOutput -> IO Bool
output renderSystem _ oos = do
    clear target black
    mapM_ (\oo -> render (ooState oo)) (elemsIL oos) -- render 'State'!
    display target
    newTexture <- getRenderTexture target
    setTexture (renderSprite renderSystem) newTexture False
    draw (renderWindow renderSystem) (renderSprite renderSystem) Nothing
    display (renderWindow renderSystem)
    return $ null $ keysIL oos
  where
    target :: RenderTexture
    target = renderTexture renderSystem
    render :: State -> IO ()
    render (Entity pos (RenderableSprite sprite) rotation color) = do
        setPosition sprite (yampaToSfVectorRounded pos)

        let quantizeRotation r = ((360/steps)*).fromIntegral.round$r/(360/steps)
            steps = 24
        setRotation sprite (quantizeRotation rotation)

        setColor sprite color
        draw target sprite Nothing
        return ()
    render (Entity pos (RenderableText text string) rotation color) = do
        setPosition text (yampaToSfVectorRounded pos)
        setRotation text rotation
        setTextString text string
        setTextColor text color
        draw target text Nothing
        return ()
    render (Camera pos) = do
        currentView <- getView target
        setViewCenter currentView $ (\(Vec2f x y) ->
            Vec2f (x) (y+0.5)) (yampaToSfVectorRounded pos)
        setView target currentView
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

rotationToVelocity :: Velocity2 -> Double -> Double -> Velocity2
rotationToVelocity velocity_base rotation velocity_normal =
    velocity_base ^+^ (velocity_normal*^(radToVec rotation))

playerObject :: RandomGen r =>
    Position2 -> Sprite -> Sprite -> Color -> Sprite -> Sprite -> r -> Object
playerObject p0 sprite_still sprite_move color bullet_spr smoke_spr gen =

    let pos_speed = 200
        pos_drag = 0.2
        rot_speed = 50
        rot_drag = 10
        smoke_particles = 160

    in proc objEvents -> do

    -- Assign numerical values to each key used to control the player
    left  <- applyValue (-1) 0 <<< trackKey KeyLeft  -< (oeInput objEvents)
    right <- applyValue  1   0 <<< trackKey KeyRight -< (oeInput objEvents)
    up    <- applyValue  1   0 <<< trackKey KeyUp    -< (oeInput objEvents)

    -- If the 'up key' is pressed, the engines are firing (boolean signal)
    engines_on <- trackKey KeyUp -< (oeInput objEvents)

    -- Change the sprite to the 'moving' sprite if the engines are firing
    sprite <- applyValue sprite_move sprite_still -< engines_on

    -- Set up a smoke spawning event source
    smokeSource <- occasionally gen (1/smoke_particles) () -< ()
    let createSmokeEvent = gate smokeSource engines_on

    -- Movement and rotation rules
    rec
        rot_acc <- identity -< (rot_speed *) $ (left + right)
        pos_acc <- identity -<  ((up * pos_speed) *^) (radToVec rot)

        rot_vel <- integral -< rot_acc - (rot_drag * rot_vel)
        pos_vel <- integral -< pos_acc ^-^ (pos_drag *^ pos_vel)

        rot <- integral -< rot_vel
        p <- (p0^+^) ^<< integral -< pos_vel

    -- Create a bullet when space is pressed
    createBulletEvent <- edge <<< trackKey KeySpace -< (oeInput objEvents)

    -- Create various random parameters used for smoke instantiation
    smokeEngine <- ((*0.5).(`subtract`1).fromIntegral.round) ^<< noiseR (0 :: Float, 2) gen -< ()
    smokeOffset <- noiseR ((-0.1), 0.1) gen -< ()
    let smokePosition = p ^+^ (17 *^ (radToVec (rot+pi+smokeOffset+smokeEngine)))
    smokeRedness <- noiseR (0, 255) gen -< ()

    -- Return the new object state, spawn smoke & bullets etc
    returnA -< defaultObjOutput {
        ooState = Entity
                    p
                    (RenderableSprite sprite)
                    ((90+).radToDeg.double2Float$rot)
                    color,
        ooSpawnRequests = catEvents $ [
            createBulletEvent `tag`
                particleObject
                    bullet_spr
                    p
                    (rotationToVelocity pos_vel rot 400)
                    white
                    5,
            createSmokeEvent `tag`
                particleObject
                    smoke_spr
                    smokePosition
                    (negateVector (rotationToVelocity pos_vel rot 40))
                    (Color 255 (fromIntegral (smokeRedness :: Int)) 0 255)
                    0.5
                                      ]
        }

particleObject :: Sprite -> Position2 -> Velocity2 -> Color -> Double -> Object
particleObject sprite pos vel color fade_time = proc objEvents -> do
    pos_vel <- constant vel -< ()
    pos_out <- (pos^+^) ^<< integral -< pos_vel
    decay <- (+1.0) ^<< integral -< (-1/fade_time)

    decayed_event <- edge <<^ (< 0.3) -< decay

    let fade :: Double -> Color
        fade v = color { a = ((round.(*255)) v) }

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

cameraObject :: Vector2 Double -> Object
cameraObject p0 = proc objEvents -> do

    left  <- applyValue (-1) 0 <<< trackKey KeyA -< (oeInput objEvents)
    right <- applyValue  1   0 <<< trackKey KeyD -< (oeInput objEvents)
    up    <- applyValue (-1) 0 <<< trackKey KeyW -< (oeInput objEvents)
    down  <- applyValue   1  0 <<< trackKey KeyS -< (oeInput objEvents)

    let pos_speed = 800
        pos_drag = 5

    rec
        pos_acc <- identity -<  (pos_speed *^) $ vector2 (left + right) (up + down)
        pos_vel <- integral -< pos_acc ^-^ (pos_drag *^ pos_vel)
        p <- (p0^+^) ^<< integral -< pos_vel

    returnA -< defaultObjOutput { ooState = Camera p }

-- Title:    Yampa/SFML Stub

{-# LANGUAGE Arrows #-}

module Main where

import IdentityList

import Data.Maybe
import Control.Monad.Loops

import GHC.Float

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import SFML.Graphics
import SFML.Window
import SFML.System

-- types --------

type Position2 = Point2  Double
type Velocity2 = Vector2 Double

type Input = [SFEvent]    -- non-deterministic events from input devices
type Logic = Yampa.Event () -- deterministic events from object processor

data ObjEvents = ObjEvents
    { oeInput :: Input
    , oeLogic :: Logic
    } deriving (Show)

data State = Circle Position2 CircleShape Color | Debug String

instance Show State where
    show (Circle pos shape color)
      = "Circle " ++ (show pos) ++ " " ++ (show color)
    show (Debug str)
      = str

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

main :: IO ()
main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <-
        createRenderWindow
            (VideoMode 96 96 32)
            "Yampa/SFML Stub"
            [SFDefaultStyle]
            ctxSettings
    windowSize <- getWindowSize wnd
    putStrLn $ "Got window - size: " ++ (show $ windowSize)
    clock <- createClock

    circle <- err $ createCircleShape
    setRadius circle 8
    reactimate (initialize wnd)
               (input wnd clock)
               (output wnd)
               (process (initialObjects circle))
    destroy circle
    destroy clock
  where
    initialObjects ci =
        (listToIL [playerObj, obstacleObj])
      where
        playerObj = playerObject (Point2 16 16) ci black
        obstacleObj = staticObject (Point2 48 48) ci blue

-- reactimation IO ----------

squashEvents :: [Maybe SFEvent] -> Maybe [SFEvent]
squashEvents = sequence

allPendingEvents :: RenderWindow -> IO Input
allPendingEvents wnd = do
    eventMaybe <- unfoldWhileM (/= Nothing) (pollEvent wnd)
    let events = (fromMaybe []) $ squashEvents eventMaybe
        in do
            putStrLn $ "All pending events (sense): " ++ show events
            return events


initialize :: RenderWindow -> IO Input
initialize wnd = do
    putStrLn "Initialize..."
    allPendingEvents wnd

input :: RenderWindow -> Clock -> Bool -> IO (DTime, Maybe Input)
input wnd clk _ = do
    putStrLn "Input (sense)..."
    events <- allPendingEvents wnd
    delta <- fmap (float2Double.asSeconds) (getElapsedTime clk)
    restartClock clk
    return (delta, Just events)

output :: RenderWindow -> Bool -> IL ObjOutput -> IO Bool
output wnd _ oos = do
    putStrLn "Output (actuate)..."
    clearRenderWindow wnd green
    mapM_ (\oo -> render (ooState oo)) (elemsIL oos) -- render 'State'!
    display wnd
    return $ null $ keysIL oos
  where
    render :: State -> IO ()
    render (Circle pos circle color) = do
        setPosition circle (Vec2f (double2Float$point2X pos) (double2Float$point2Y pos))
        setFillColor circle color
        draw wnd circle Nothing
        return ()
    render (Debug s) = putStrLn s


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
    (Circle p1 _ _) `hit` (Circle p2 _ _) = p1 == p2
    _ `hit` _ = False

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

playerObject :: Position2 -> CircleShape -> Color -> Object
playerObject p0 circle color = proc objEvents -> do
    -- .+^ is Point-Vector-addition
    -- ^+^ is Vector-Vector addition
    -- here we sum up all vectors based on the possibly multiple
    -- user inputs, thus allowing diagonal moves
    let target_move = foldl (^+^) (vector2 0 0) $ mapMaybe checkKey (oeInput objEvents)
    p <- (p0 .+^) ^<< integral -< 1000 *^ target_move
    returnA -< defaultObjOutput { ooState = Circle p circle color }
    where
        checkKey evt@(SFEvtKeyPressed _ _ _ _ _)
            | code evt == KeyUp    = Just $ vector2    0 (-32)
            | code evt == KeyDown  = Just $ vector2    0   32
            | code evt == KeyRight = Just $ vector2   32    0
            | code evt == KeyLeft  = Just $ vector2 (-32)   0
            | otherwise = Nothing
        checkKey _ = Nothing

staticObject :: Position2 -> CircleShape -> Color -> Object
staticObject p0 circle color = proc objEvents -> do
    returnA -< defaultObjOutput { ooState         = Circle p0 circle color
                                , ooKillRequest   = (oeLogic objEvents)
                                , ooSpawnRequests = (debugIfKilled objEvents)
                                }
  where
    debugIfKilled objEvents =
        case (oeLogic objEvents) of
            Yampa.Event () -> Event [debugObject "hit"]
            _              -> Event []

debugObject :: String -> Object
debugObject s = proc objEvents -> do
    returnA -< defaultObjOutput { ooState       = Debug s
                                , ooKillRequest = Event ()
                                }

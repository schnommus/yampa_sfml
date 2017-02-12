module Types where

import FRP.Yampa as Yampa
import FRP.Yampa.Geometry
import SFML.Graphics
import SFML.Window

type Position2 = Vector2 Double
type Velocity2 = Vector2 Double
type Rotation = Float
type UIEntity = Bool

type Input = [SFEvent]    -- non-deterministic events from input devices
type Logic = Yampa.Event () -- deterministic events from object processor

data ObjEvents = ObjEvents
    { oeInput :: Input
    , oeLogic :: Logic
    } deriving (Show)

data Renderable = RenderableSprite Sprite | RenderableText Text String

data State = Entity Position2 Renderable Rotation Color UIEntity |
             Camera Position2

instance Show State where
    show (Entity _ _ _ _ _) = "Entity"
    show (Camera _) = "Camera"

data ObjOutput = ObjOutput
    { ooLayer         :: Double
    , ooState         :: State
    , ooKillRequest   :: Yampa.Event ()       -- NoEvent|Event ()
    , ooSpawnRequests :: Yampa.Event [Object]
    } deriving (Show)

defaultObjOutput = ObjOutput
    { ooLayer         = 1.0
    , ooState         = undefined
    , ooKillRequest   = Yampa.NoEvent
    , ooSpawnRequests = Yampa.NoEvent
    }

type Object = SF ObjEvents ObjOutput

-- utility ---------

instance Show (SF a b) where
    show sf = "SF"

data RenderSystem = RenderSystem
    { renderWindow :: RenderWindow
    , renderTexture :: RenderTexture
    , renderSprite :: Sprite
    }

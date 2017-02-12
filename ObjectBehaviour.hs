{-# LANGUAGE Arrows #-}

module ObjectBehaviour where

import ObjectHelpers
import Types

import FRP.Yampa              as Yampa
import FRP.Yampa.Random
import FRP.Yampa.Geometry
import SFML.Graphics
import SFML.Window
import GHC.Float

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
                    color
                    False,
        ooSpawnRequests = catEvents $ [
            createBulletEvent `tag`
                particleObject
                    bullet_spr
                    p
                    (rotationToVelocity pos_vel rot 400)
                    white
                    5
                    1.0,
            createSmokeEvent `tag`
                particleObject
                    smoke_spr
                    smokePosition
                    (negateVector (rotationToVelocity pos_vel rot 40))
                    (Color 255 (fromIntegral (smokeRedness :: Int)) 0 255)
                    0.5
                    1.0
                                      ]
        }

particleObject :: Sprite -> Position2 -> Velocity2 -> Color -> Double -> Double -> Object
particleObject sprite pos vel color fade_time layer = proc objEvents -> do
    pos_vel <- constant vel -< ()
    pos_out <- (pos^+^) ^<< integral -< pos_vel
    decay <- (+1.0) ^<< integral -< (-1/fade_time)

    decayed_event <- edge <<^ (< 0.3) -< decay

    let fade :: Double -> Color
        fade v = color { a = ((round.(*255)) v) }

    returnA -< defaultObjOutput {
        ooLayer = layer,
        ooState = Entity pos_out (RenderableSprite sprite) 0 (fade decay) False,
        ooKillRequest = decayed_event
                                }

fpsObject :: Vector2 Double -> Text -> Color -> Object
fpsObject pos text color = proc objEvents -> do
    rec
        elapsed <- localTime -< ()
        oldElapsed <- iPre 0.1 -< elapsed
        fps <- arr ((++" FPS").show.round.(1.0/)) -< elapsed - oldElapsed
    returnA -< defaultObjOutput {
        ooState = Entity pos (RenderableText text fps) 0 color True
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

    returnA -< defaultObjOutput { ooLayer = -1000, ooState = Camera p }

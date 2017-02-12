{-# LANGUAGE Arrows #-}

module ObjectHelpers where

import Types

import FRP.Yampa as Yampa
import FRP.Yampa.Geometry
import SFML.Graphics
import SFML.Window
import Data.List.Safe as ListSafe
import Data.Maybe

centerSprite :: Sprite -> IO ()
centerSprite sprite = do
    bounds <- getLocalBounds sprite
    setOrigin sprite (Vec2f ((/2).fwidth$bounds) ((/2).fheight$bounds))

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


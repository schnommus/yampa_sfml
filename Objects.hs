module Objects where

import Types
import ObjectHelpers
import ObjectBehaviour

import FRP.Yampa.Geometry
import FRP.Yampa.Random
import SFML.Graphics
import System.Random
import Control.Applicative

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

starObjs :: IO [Object]
starObjs = do
    star_spr <- loadPixelSprite "media/pixelstar.png" (IntRect 0 0 1 1)

    let num_stars = 100
        width = 500

        random_vector = liftA2 vector2 (randomRIO (-width, width)) (randomRIO (-width, width))
        random_layer = randomRIO (1.5, 3)
        star_init = liftA2 (,) random_vector random_layer
        create_star (pos,layer) = particleObject star_spr pos (vector2 0 0) white 1e9 layer

    sequenceA $ fmap ($star_init) (replicate num_stars (liftA create_star))

cameraObj :: IO Object
cameraObj = do
    return $ cameraObject (vector2 0 0)

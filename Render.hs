module Render where

import Types
import IdentityList

import FRP.Yampa.Geometry
import SFML.Graphics
import SFML.Window
import SFML.System
import Data.Maybe
import Data.List
import GHC.Float

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

data RenderableViews = RenderableViews { renderableuiView :: View
                                       , renderablerealView :: View
                                       , renderablerealViewPos :: Position2}

output :: RenderSystem -> Bool -> IL ObjOutput -> IO Bool
output renderSystem _ oos = do
    clear target black

    -- Camera logic
    realView <- getView target >>= copyView
    uiView <- getDefaultView target

    let process_camera c = case (ooState c) of
            (Camera pos) -> Just pos
            _ -> Nothing
        real_camera_pos = Prelude.head $ catMaybes $ map process_camera (elemsIL oos)

    setViewCenter realView $ (\(Vec2f x y) ->
        Vec2f (x) (y+0.5)) (yampaToSfVectorRounded real_camera_pos)

    -- Render everything
    let ordered states = sortBy (\a b -> ooLayer b `compare` ooLayer a) states
    let views = RenderableViews realView uiView real_camera_pos
    mapM_ (\oo -> render views (ooState oo) (ooLayer oo)) (ordered$elemsIL oos)

    -- Swap buffers and render the scaled texture
    display target
    newTexture <- getRenderTexture target
    setTexture (renderSprite renderSystem) newTexture False
    draw (renderWindow renderSystem) (renderSprite renderSystem) Nothing
    display (renderWindow renderSystem)

    -- Destroy the view we copied
    destroy realView

    return $ null $ keysIL oos
  where
    target :: RenderTexture
    target = renderTexture renderSystem

    yampaToSfVectorRounded :: Vector2 Double -> Vec2f
    yampaToSfVectorRounded v = Vec2f (f$vector2X v) (f$vector2Y v)
        where f = fromIntegral.round

    sfVectorToYampa :: Vec2f -> Vector2 Double
    sfVectorToYampa (Vec2f x y) = vector2 (float2Double x) (float2Double y)

    layerScale :: Vector2 Double -> Double -> Vector2 Double
    layerScale pos_in layer = vector2 ((vector2X pos_in)/layer) ((vector2Y pos_in)/layer)

    do_draw ::  (SFDrawable a, SFTransformable a) =>
        RenderableViews -> a -> RenderTexture -> Position2 -> Double -> UIEntity -> IO ()
    do_draw (RenderableViews realView _ view_pos) drawable target pos layer False = do
        setPosition drawable (yampaToSfVectorRounded $
            (layerScale (pos ^-^ view_pos) layer))
        setView target realView
        draw target drawable Nothing
    do_draw (RenderableViews _ uiView _) drawable target pos layer True = do
        setPosition drawable (yampaToSfVectorRounded $ pos)
        setView target uiView
        draw target drawable Nothing

    render :: RenderableViews -> State -> Double -> IO ()
    render views (Entity pos (RenderableSprite sprite) rotation color is_ui) layer = do
        let quantizeRotation r = ((360/steps)*).fromIntegral.round$r/(360/steps)
            steps = 24

        setRotation sprite (quantizeRotation rotation)

        setColor sprite color

        do_draw views sprite target pos layer is_ui
    render views (Entity pos (RenderableText text string) rotation color is_ui) layer = do
        setRotation text rotation
        setTextString text string
        setTextColor text color
        do_draw views text target pos layer is_ui
    render views (Camera pos) layer = do
        return ()

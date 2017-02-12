module Main where

import Types
import Render
import Objects
import Reactimation
import IdentityList

import FRP.Yampa
import SFML.System

main :: IO ()
main = do
    renderSystem <- init_rendersystem

    clock <- createClock
    player <- playerObj
    fps <- fpsObj
    camera <- cameraObj
    stars <- starObjs

    let ents = [player, fps, camera] ++ stars

    reactimate (initialize $ renderWindow renderSystem)
               (input (renderWindow renderSystem) clock)
               (output renderSystem)
               (process (listToIL ents))

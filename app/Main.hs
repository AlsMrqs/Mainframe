module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Engine.Control
import Engine.Render

-- Start --
main :: IO ()
main = do
    (progname,_) <- getArgsAndInitialize
    _window      <- createWindow "Vector Field"
    displayCallback       $= addTimerCallback 60 (render)
    keyboardMouseCallback $= Just (keyboardMouse)
    mainLoop
    

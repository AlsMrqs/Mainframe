module Main where
import Graphics.Rendering.OpenGL hiding (bitmap,Program)
import Graphics.UI.GLUT hiding (Text,bitmap,Program)
import Control.Concurrent
import System.IO
import System.Process
import Struct.Manager as Manager
import Struct.Control as Control
import Struct.Engine  as Engine
import Struct.Render  as Render
import Struct.System  as System
import Struct.Screen  as Screen

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    _window      <- createWindow progName
    mainframe    <- newMVar (Engine.runtime)
    windowSize            $= Size 600 500
    displayCallback       $= (Render.display     mainframe)
    keyboardMouseCallback $= Just (keyboardMouse mainframe)
    motionCallback        $= Just (motion        mainframe)
    mouseCallback         $= Just (mouse         mainframe)
    passiveMotionCallback $= Just (passiveMotion mainframe)
    mainLoop


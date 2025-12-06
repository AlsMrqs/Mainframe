module Program.Bitmap where

import Graphics.Rendering.OpenGL hiding (Program, bitmap)
import Graphics.UI.GLUT hiding (Program, bitmap)

import Control.Concurrent.MVar

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.IORef

import Engine.Core
-- import qualified Engine.Render as Render
-- import qualified Engine.Control as Control

import Program.Bitmap.Design
import Program.Bitmap.Script.Display
import Program.Bitmap.Script.KeyboardMouse
import Program.Bitmap.Script.Motion
import Program.Bitmap.Script.Mouse

start :: Maybe Node -> IO ()
start callerNode= do
    bitmapWindow <- createWindow "Bitmap"

    engine_    <- newMVar $ Node bitmap callerNode []
    windowFlag <- newIORef 1

    currentWindow         $= Just bitmapWindow
    postRedisplay (Just bitmapWindow)
    displayCallback       $= (bitmapDisplay bitmapWindow windowFlag engine_)
    keyboardMouseCallback $= Just (bitmapKeyboardMouse (bitmapWindow, windowFlag) $ engine_)
    mouseCallback         $= Just (bitmapMouse engine_)
    motionCallback        $= Just (bitmapMotion engine_)

bitmapNode = Node
    { program = bitmap
    , caller = Maybe.Nothing
    , link = [] }

bitmap :: Program
bitmap = Program
    { name          = "bitmap"
    , call          = start
    -- Script
    , keyboardMouse = bitmapKeyboardMouse
    , mouse         = bitmapMouse 
    , motion        = bitmapMotion 
    , passiveMotion = bitmapPassiveMotion
    -- Output
    , frame         = gridLines 
    , polygons      = Set.empty 
    , object        = Map.empty
    , element       = Map.empty
    -- Database
    , terminal      = CLI False [] [[]]
    , sprites       = Map.empty }


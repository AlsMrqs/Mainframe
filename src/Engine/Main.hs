module Engine.Main where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import Control.Concurrent.MVar
import Control.Concurrent

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.IORef

import Engine.Control
import Engine.Render
import Engine.Core
-- import qualified Engine.Math.Space as Math

import qualified Program.Bitmap as Bitmap -- Engine.Programs!!!!

root :: Node
root = Node
    { program = engine
    , caller  = Maybe.Nothing
    , link    = [Bitmap.bitmapNode] }

engine:: Program
engine = Program
    { name          = "engine"
    -- Executable
    , call          = start

    -- Controller
    , keyboardMouse = engineKeyboardMouse
    , mouse         = engineMouse
    , motion        = engineMotion
    , passiveMotion = enginePassiveMotion

    -- Screen
    , frame         = []
    , polygons      = Set.empty
    , object        = Map.singleton (Lines,bitmapButton) ("Bitmap.start" {- :: IO ()-})
    , element       = Map.singleton "Bitmap.Button" bitmapButtonTest

    -- Database
    , terminal      = CLI False [] [[]]
    , sprites       = Map.empty }

bitmapButton :: [(GLfloat, GLfloat, GLfloat)]
-- bitmapButton = [(0,0,0),(0,0.15,0),(0.25,0.15,0),(0.25,0,0)]
bitmapButton = [(0,0,0), (0.1,0.1,0)] -- , (0.2,0,0), (0.3,0.1,0), (0.4,0,0), (0.5,0.1,0)]

bitmapButtonTest = Element 
    { code      = "Bitmap.start"
    , mode      = Polygon
    , theColor  = Color3 0.2 0.0 (1.0 :: GLfloat)
    , location  = [(-0.5,0.5,0),(-0.4,-0.5,0),(-0.3,0.5,0),(-0.2,-0.5,0),(-0.1,0.0,0)]
    , mouseOver = Nothing
    , onClick   = Nothing }

setColor :: Color3 GLfloat -> Element -> Element
setColor clr elm = elm { theColor = clr }

-- mouseOver_ :: Position -> Element -> Element
-- mouseOver_ (Position x y) el = 

-- adds a Bitmap Button!
start :: Maybe Node -> IO ()
start caller_ = do
    engineWindow <- createWindow "Engine"
    currentWindow $= Just engineWindow
    windowSize    $= Size 800 600

    engine_ <- newMVar $ root {caller = caller_}
    ref <- newIORef 1

    displayCallback       $= (engineDisplay engineWindow engine_)
    keyboardMouseCallback $= Just ((keyboardMouse . program $ root) (engineWindow, ref) $ engine_)
    -- rewrite the functions !!!
    mouseCallback         $= Just ((mouse . program $ root) engine_)
    motionCallback        $= Just ((motion . program $ root) engine_)
    passiveMotionCallback $= Just ((passiveMotion . program $ root) engine_)
    


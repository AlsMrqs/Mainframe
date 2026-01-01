module Program.Engine.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process

import qualified Data.Set as Set
import qualified Data.Map as Map

import Struct.System as System
import Struct.Program as Program

display :: MVar (Program.System) -> DisplayCallback
display systemMVar = do
    clear [ColorBuffer]
    color $ Color3 0 1 (1:: GLfloat)
    renderPrimitive Points . mapM_ draw_ $ [(0,0,0)]
    swapBuffers
    postRedisplay Nothing

draw_ :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
draw_ (x,y,z) = vertex $ Vertex3 x y z

pointToGLPoint_ :: (Double, Double, Double) -> (GLfloat, GLfloat, GLfloat)
pointToGLPoint_ (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)


module Program.Bitmap.Script.Mouse where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.MVar

import Engine.Core
import Program.Bitmap.Design

bitmapMouse :: MVar Node -> MouseCallback
bitmapMouse nodeMVar button state pos = do
    putStrLn $ "Mouse Button: " ++ show button
    putStrLn $ "Mouse State: " ++ show state
    putStrLn $ "Mouse Position: " ++ show pos
    putStrLn $ "Mouse Quadrant: " ++ show (quadrant pos)
    case state of
        Down -> takeMVar nodeMVar >>= putMVar nodeMVar .
            (maybe id (\f -> f pos) $ Map.lookup (button, state) mouseActions)
        _    -> return ()

mouseActions :: Map.Map (MouseButton, KeyState) (Position -> Node -> Node)
mouseActions = Map.fromList
    [ ((LeftButton, Down), insertPolygon) 
    , ((RightButton, Down), removePolygon) ]

insertPolygon :: Position -> Node -> Node
insertPolygon pos node = 
    node {
        program = (program node) {
            polygons = Set.insert (paintOut pos) (polygons $ program node)
        }
    }

removePolygon :: Position -> Node -> Node
removePolygon pos node = 
    node {
        program = (program node) {
            polygons = Set.delete (paintOut pos) (polygons $ program node)
        }
    }


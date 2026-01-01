module Struct.Render where
import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)
import Control.Concurrent.MVar
-- import Struct.Program as Program
import Struct.System  as System 
import Struct.Engine  as Engine
import Struct.Screen  as Screen
import Struct.Space   as Space
import Struct.DBMS    as DBMS

prism :: Screen.Element -> RGB Screen.Flipside
prism = Screen._RGB_

flash :: [(Space.Coord, Space.Coord, Space.Coord)] -> IO ()
flash = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)

display :: MVar (Program) -> DisplayCallback
display _root = do
    clear [ColorBuffer]

    program <- readMVar _root

    let engine = System.vert program
    let entity = System.vert engine     {- SW | W | NW | N | NE | E | SE | S -}
    let spectr = System.vert entity
    
    color (prism spectr)
    renderPrimitive (model spectr) $ flash (Engine.constellation spectr)

    swapBuffers
    postRedisplay Nothing


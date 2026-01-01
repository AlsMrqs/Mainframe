module Struct.Control where
import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)
import Control.Concurrent.MVar
-- import Struct.Program as Program
import Struct.Compass as Compass
import Struct.System  as System 
import Struct.Engine  as Engine
import Struct.Space   as Space
import Struct.DBMS    as DBMS
import Struct.Math    as Math

keyboardMouse :: MVar (Program) -> KeyboardMouseCallback
keyboardMouse _root key keyState _mod pos = return () 

mouse :: MVar (Program) -> MouseCallback
mouse _root button keyState pos = return () 

motion :: MVar (Program) -> MotionCallback
motion _root pos = return () 

passiveMotion :: MVar (Program) -> MotionCallback
passiveMotion _root _pos = do 
    (Size n m)         <- get windowSize
    let (Position x y) = _pos
    let vertice        = fromIntegral(m-y) :: Space.Coord
        ordinat        = fromIntegral(m)   :: Space.Coord
        horizon        = fromIntegral(x)   :: Space.Coord
        absciss        = fromIntegral(n)   :: Space.Coord
    --  aim :: Math.Point = (Space.Coord, Space.Coord, Space.Coord) 
    let aim = (,,) 
            ((+) (-1) . (*) horizon $ (/) 2.0 (absciss))
            ((+) (-1) . (*) vertice $ (/) 2.0 (ordinat)) (0)

    program    <- takeMVar _root     -- DBMS.DBMS (..)
    let engine = System.vert program -- Compass.Compass (Screen.Interactive Math.Point)
    let entity = System.vert engine  -- Screen.Interactive Math.Point
    let spectr = System.vert entity  -- Screen.Element

    let emotion = System.cross aim entity -- :: Screen.Interactive Math.Point
        ansswer = case emotion of
            Nothing -> engine             -- :: Compass.Compass (Engine.Entity)
            Just v  -> Compass.insert v engine 
    putMVar _root (DBMS.insert ansswer program) -- :: DBMS.DBMS (..)
    print aim


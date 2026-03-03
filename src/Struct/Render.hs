module Struct.Render where
import Graphics.Rendering.OpenGL hiding (Program)
-- import Graphics.UI.GLUT          hiding (Program)
-- import Control.Concurrent.MVar
-- import qualified Struct.Math     as Math
-- import Struct.Triangle as Triangle
import qualified Struct.Space as Space
import qualified Struct.Screen as Screen
-- import Struct.Cube     as Cube
-- import Struct.Mouse    as Mouse
-- import Struct.Program  as Program

prism :: Screen.Element -> Screen.RGB Screen.Flipside
prism = Screen._RGB_

flash :: [Space.Point] -> IO ()
flash = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)

constellation :: Screen.Element -> [Space.Point]
constellation = Screen.ordin

-- display :: DisplayCallback
-- display _root = do
--     clear [ColorBuffer]
-- 
--     _bitmap <- readMVar _root
--     color $ BitMap.currentRGB _bitmap
--     let elementList = BitMap.bitSet _bitmap
-- 
--     mapM_ (\el -> renderPrimitive  (Screen.model el) 
--         $ flash (constellation el)) elementList
-- 
--     swapBuffers
--     postRedisplay Nothing
    

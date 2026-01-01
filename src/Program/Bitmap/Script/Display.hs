module Program.Bitmap.Script.Display where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.MVar
import qualified Engine.Core as Core
import qualified Data.Set as Set
import Data.IORef

type Point = (Double,Double,Double)

draw_ :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
draw_ (x,y,z) = vertex $ Vertex3 x y z

pointToGLPoint_ :: Point -> (GLfloat, GLfloat, GLfloat)
pointToGLPoint_ (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

bitmapDisplay :: Window -> IORef Int -> MVar (Core.Node) ->  DisplayCallback
bitmapDisplay wind windowFlag nodeMVar = do
    node <- readMVar nodeMVar

    {- Design the Software -}
    clear [ColorBuffer]
    color $ Color3 0 0 (1 :: GLfloat)
    renderPrimitive Lines . mapM_ draw_ $ Core.frame (Core.program node) 
    color $ Color3 1 1 (1 :: GLfloat)

    -- Here is a Data.Set now!!
    mapM_ (renderPrimitive Polygon . mapM_ draw_) . Set.toList 
        $ Core.polygons (Core.program node)
    putStrLn "display Bitmap!"

    swapBuffers
    postRedisplay Nothing

module Render where

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as OpenGL hiding (Program)
import qualified Space as Space
import qualified Screen as Screen

prism :: Screen.Element -> Screen.RGB Screen.Flipside
prism = Screen._RGB_

flash :: [Space.Point] -> IO ()
flash = mapM_ (\(x,y,z) -> OpenGL.vertex $ OpenGL.Vertex3 x y z)

constellation :: Screen.Element -> [Space.Point]
constellation = Screen.ordin

renderElement :: Screen.Element -> IO ()
renderElement x = do
    GLUT.color (Screen._RGB_ x)
    OpenGL.renderPrimitive (Screen.model x) $ flash (constellation x)
    return ()


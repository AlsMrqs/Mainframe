module Engine.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type Point = (Double, Double, Double)
type Vector = Point

-- Render --
render :: DisplayCallback
render = do
    clear [ColorBuffer]
    color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    renderPrimitive Lines . mapM_ (draw . pointToGLPoint) $ 
        [(-0.5,0,0),(0.5,0,0),(0,-0.5,0),(0,0.5,0),(0,0,-0.5),(0,0,0.5)]
    swapBuffers 
    postRedisplay Nothing
    where
        draw :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
        draw (x,y,z) = vertex $ Vertex3 x y z

        pointToGLPoint :: Point -> (GLfloat, GLfloat, GLfloat)
        pointToGLPoint (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)


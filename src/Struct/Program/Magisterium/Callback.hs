module Struct.Program.Magisterium.Callback 
    ( module Magisterium
    , display )
where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Struct.Program.Magisterium.Magisterium as Magisterium
import qualified Struct.Render as Render
import qualified Struct.Screen as Screen
import qualified Struct.Math as Math
import qualified Struct.Space as Space
import qualified Control.Concurrent as Concurrent

rgb                  = OpenGL.Color3 0 1 (0 :: GLUT.GLfloat)
renderSpacePoints    = (Render.renderElement . Screen.pointsToElement rgb)

rgb'                 = OpenGL.Color3 1 1 (0 :: GLUT.GLfloat)
renderSpaceLines     = (Render.renderElement . Screen.linesToElement rgb')

display :: Magisterium.Game -> GLUT.DisplayCallback
display game = do
    GLUT.clear [GLUT.ColorBuffer]
    t <- Math.getTime
    case (projectile . offensive . order) game of
        Nothing -> return ()
        Just (t0,f) -> maybe (pure ()) renderSpacePoints (Magisterium.trace f (t0/100) (t/100))
    return ()



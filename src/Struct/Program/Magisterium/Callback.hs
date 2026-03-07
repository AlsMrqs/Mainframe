module Struct.Program.Magisterium.Callback 
    ( module Magisterium
    , display )
where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Struct.Program.Magisterium.Magisterium as Magisterium
import qualified Struct.Render as Render
import qualified Struct.Screen as Screen

-- game status
display :: Magisterium.Magisterium -> GLUT.DisplayCallback
display magist = do
    GLUT.clear [GLUT.ColorBuffer]

    GLUT.color (OpenGL.Color3 0.1 0.1 (0.1 :: GLUT.GLfloat)) 
    let elementList = Magisterium.drawCartesian (Magisterium.Ground (5,5)) --(fl4g{})
    
    mapM_ (\el -> OpenGL.renderPrimitive  (Screen.model el) 
        $ Render.flash (Render.constellation el)) elementList

    ----------------------- Test!!! ---------------
    let expr = (Magisterium.expression . Magisterium.player1) magist
    GLUT.color (OpenGL.Color3 0.1 1 (0.1 :: GLUT.GLfloat)) 
    mapM_ (\el -> OpenGL.renderPrimitive  (Screen.model el) 
        $ Render.flash (Render.constellation el)) [Magisterium.plot expr]
    ------------------------------------------------

    return ()


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

display :: Magisterium.Game -> GLUT.DisplayCallback
display game = do
    GLUT.clear [GLUT.ColorBuffer]
    maybe (return ()) traceFunction 
        $ (Magisterium.projectile . Magisterium.player1) game
    -- player2 (Need TIME) !!!!
    where
    rgb                  = OpenGL.Color3 0 1 (0 :: GLUT.GLfloat)
    renderSpacePoints    = (Render.renderElement . Screen.pointsToElement rgb)
    rgb'                 = OpenGL.Color3 1 1 (0 :: GLUT.GLfloat)
    renderSpaceLines     = (Render.renderElement . Screen.linesToElement rgb')
    -- traceFunction (t0,f) = maybe (return ()) renderSpacePoints
    --     . Magisterium.trace f t0 =<< Math.getTime
    traceFunction (t0,f) = do
        t <- Math.getTime
        let dt = (t/100) - (t0/100)
            x  = (-) 1 dt
        maybe (return ()) renderSpacePoints
            . Magisterium.trace f t0 $ t
        renderSpaceLines
            $ Magisterium.aim f $ (-) 1 ((t-t0)/100)
        -------------{ Tangent }----------------
        maybe (return ()) renderSpacePoints
            . Magisterium.trace (Magisterium.tangent x f) (1e2) $ (2.99e2)
        -------------{ Perpendicular }----------------
        maybe (return ()) (renderSpacePoints . map (Math.sumPoint (x, apply f x, 0)))
            . Magisterium.trace (Magisterium.perpendicular x f) (1e2) $ (2.99e2)
        -------------{ Player2 }-------------------
        maybe (return ())
              (renderSpaceLines 
                . (\fP2 -> Space.flatLine (x,apply f x,0) 0.3 $ atan(apply fP2 x)) )
            $ (Magisterium.analysis . Magisterium.player2) game


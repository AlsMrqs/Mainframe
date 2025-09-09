module Engine.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent

import Engine.Universe
import Engine.Obj
import Graph.Grammar
import Parser

-- Render --
render :: MVar (PreVector, [Symbol]) -> DisplayCallback
render mvarParser = do
    clear [ColorBuffer]

    color $ Color3 1 0 (0 :: GLfloat)

    (a,b,c) <- readMVar mvarParser >>= return . forest . fst
    -- putStrLn $ "Magnitude X: "++(show $ magnitude a)
    -- putStrLn $ "Magnitude Y: "++(show $ magnitude b)
    -- putStrLn $ "Magnitude Z: "++(show $ magnitude c)
    putStrLn $ show (a,b,c)

    let range = [-0.5,(-0.5)+2e-1..0.5]
        charge = [(x,y,z) | x <- range, y <- range, z <- range]

    --print $ map (\i -> (calc a i, calc b i, calc c i)) [(x,y,z) | x <- range, y <- range, z <- range]
    --display . Obj Lines $ map (\i -> (calc a i, calc b i, calc c i)) [(x,y,z) | x <- range, y <- range, z <- range]
    
    display . Obj Lines $ foldr (\i acc -> i:(calc a i, calc b i, calc c i):acc) [] charge

    -- color $ Color3 0.10 0.10 (0.10 :: GLfloat)
    -- display crux
    -- color $ Color3 0.50 0.00 (0.50 :: GLfloat)
    -- mapM_ display listNumbersX
    -- mapM_ display listNumbersY
    swapBuffers 
    postRedisplay Nothing

display :: Obj -> DisplayCallback
display (Obj x1 x2) = renderPrimitive x1 $ mapM_ (draw . pointToGLPoint) x2
    where
    draw :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
    draw (x,y,z) = vertex $ Vertex3 x y z

    pointToGLPoint :: Point -> (GLfloat, GLfloat, GLfloat)
    pointToGLPoint (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

crux :: Obj
crux = Obj Lines [(-0.5,0,0),(0.5,0,0),(0,-0.5,0),(0,0.5,0),(0,0,-0.5),(0,0,0.5)] 

listNumbersX :: [Obj]
listNumbersX = [Obj Lines [(x,0.01,0),(x,-0.01,0)] | x <- [-0.5,-0.4..0.5]]

listNumbersY :: [Obj]
listNumbersY = [Obj Lines [(0.01,y,0),(-0.01,y,0)] | y <- [-0.5,-0.4..0.5]]


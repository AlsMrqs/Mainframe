module Engine.Render where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process

import qualified Engine.Core as Core
import Engine.Object.Particle
import Engine.Math.Space
import Engine.Control

import Compiler.Language.Dictionary
import Compiler.Language.Grammar
import Compiler.Parser
import Compiler.Lexer
import Compiler.Solver

draw_ :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
draw_ (x,y,z) = vertex $ Vertex3 x y z

pointToGLPoint_ :: Point -> (GLfloat, GLfloat, GLfloat)
pointToGLPoint_ (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)

{- (Command Dynamics) *Script Design -} 
-- read-only
mainRender :: MVar (Core.Node) ->  DisplayCallback
mainRender nodeMVar = do
    {- Run Command -}
    node <- readMVar nodeMVar
    {- Design the Software -}
    clear [ColorBuffer]
    color $ Color3 0 0 (1 :: GLfloat)
    renderPrimitive Lines . mapM_ draw_ $ Core.frame (Core.program node) 
    swapBuffers
    postRedisplay Nothing
     
{- render is a {initial state} of {IO ()} machine -}
render :: MVar (ExprMaker, [Symbol]) -> MVar ([Char], Automaton) -> MVar Mouse -> MVar [Particle] -> DisplayCallback
render mvarParser mvarLexer mvarMouse mvarParticles = do
    clear [ColorBuffer]

    -- get (Exp,Exp,Exp) from (MVar Parser)
    (exprX, exprY, exprZ) <- readMVar mvarParser >>= return . expressions . fst

    -- get (Either (Token, Char) ([Char], Automaton) from (MVar Lexer)
    inbox   <- readMVar mvarLexer

    -- get (Mouse Memory)
    (x',y') <- readMVar mvarMouse >>= return . getMemory

    -- Console Output (Prompt)
    system "clear"
    -- putStrLn $ "("++(mathExpr exprX)++","++(mathExpr exprY)++","++(mathExpr exprZ)++")"
    -- putStrLn $ fst inbox

    -- set a (Cube Interval)
    let step = (+2.5e-1)
        limit = 0.5
        range  = [-limit,step(-limit)..limit]
        charge = [(x,y,z) | x <- range, y <- range, z <- range]

    -- set (Function of Vector Field)
        f = solve exprX
        g = solve exprY
        h = solve exprZ
        vectorFunction = assign (f,g,h)

    -- render (Cartesian Axis)
    color $ Color3 0.10 0.55 (0.55 :: GLfloat)
    display (x',y') Lines crux

    -- render (Numbers in Axis)
    color $ Color3 0.50 0.00 (0.50 :: GLfloat)
    mapM_ (display (x',y') Lines) listNumbersX 
    mapM_ (display (x',y') Lines) listNumbersY

    -- render Applyed (Function of Vector Field) in (Cube Interval)
    color $ Color3 0.1 0.1 (0.1 :: GLfloat)
    display (x',y') Lines 
        $ foldr (\i acc -> i: (sumPoint i {-$ resize 0.1 -} $ subtr i $ vectorFunction i) :acc) [] charge

    -- rendering a Particles
    particles <- readMVar mvarParticles
    newTime   <- getTime

    -- let force p      = subtr (space $ coordinate p) (vectorFunction . space $ coordinate p) 
    --     applyField p = changeSpeed (sumPoint (speed p) . divPointBy (mass p) $ force p) p

    -- putMVar mvarParticles $ map (updateCoordinate (newTime / 1e8) . applyField) particles

    color $ Color3 0 1 (1 :: GLfloat)
    display (x',y') Points $ map (space . coordinate) $ particles
    print particles
    print (newTime / 1e8)

    -- render (Callers)
    swapBuffers 
    postRedisplay Nothing

updateView :: (Double,Double) -> Point -> Point
updateView (x,y) = flip rotateY y . flip rotateX x

-- Type (Obj) NOT FOUD!!!
-- 
-- display :: (Double, Double) -> Obj -> DisplayCallback
display (a,b) x1 x2 = renderPrimitive x1 
    $ mapM_ (draw . pointToGLPoint . updateView (a,b)) x2
    where
    draw :: (GLfloat, GLfloat, GLfloat) -> DisplayCallback 
    draw (x,y,z) = vertex $ Vertex3 x y z

    pointToGLPoint :: Point -> (GLfloat, GLfloat, GLfloat)
    pointToGLPoint (x,y,z) = (realToFrac x, realToFrac y, realToFrac z)
-- 
-- crux :: Obj
crux = [(-0.5,0,0),(0.5,0,0),(0,-0.5,0),(0,0.5,0),(0,0,-0.5),(0,0,0.5)] 

-- listNumbersX :: [Obj]
listNumbersX = [[(x,0.01,0),(x,-0.01,0)] | x <- [-0.5,-0.4..0.5]]

-- listNumbersY :: [Obj]
listNumbersY = [[(0.01,y,0),(-0.01,y,0)] | y <- [-0.5,-0.4..0.5]]


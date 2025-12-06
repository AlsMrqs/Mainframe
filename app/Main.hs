module Main where

import Graphics.Rendering.OpenGL hiding (bitmap,Program)
import Graphics.UI.GLUT hiding (Text,bitmap,Program)
import Control.Concurrent
import System.IO
import System.Process
import Data.List as List hiding (singleton)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Program.Bitmap as Bitmap

import qualified Engine.Core as Core
import Engine.Object.Particle
import Engine.Math.Dynamics
import Engine.Math.Space
import Engine.Control
import Engine.Render
import Engine.Element
import qualified Engine.Main as Engine.Main
import qualified Engine.Math.Space as Math

import Kernel.Core

import Compiler.Language.Dictionary
import Compiler.Language.Grammar
import Compiler.Parser
import Compiler.Lexer
import Compiler.Solver
import Graphics.UI.GLUT

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    -- Call Engine
    -- Engine -> Calls Bitmap!!
    (Core.call . Core.program $ Engine.Main.root) Nothing -- make Bitmap -> return (Sprite)
    -- Call Bitmap
    -- (Core.call . Core.program $ Bitmap.bitmapNode) Nothing
    mainLoop

{- Main Function - The software starts here -}
-- main :: IO ()
-- main = do
--     (progName,_) <- getArgsAndInitialize
--     _window      <- createWindow progName
-- 
--     timeNow <- getTime
--     let p1 = Particle 5 (Coordinate (0,0,0) timeNow) (0,0,0)
-- 
--     lexator   <- newMVar ([], machine)
--     parseator <- newMVar (ExprMaker X (Empty,Empty,Empty), [Variable grammar])
--     rotation  <- newMVar $ Mouse (0,0) (0,0) (0,0)
--     particles <- newMVar . (: [p1]) =<< testParticle
-- 
--     tidClock  <- forkIO $ clock parseator particles
-- 
--     windowSize            $= Size 600 500
--     -- these functions .. needs change it's <arguments> for each program
--     -- render :: can have multiples screens
--     displayCallback       $= addTimerCallback 60 (render parseator lexator rotation particles)
--     keyboardMouseCallback $= Just (keyboardMouse tidClock lexator parseator)
--     motionCallback        $= Just (draggingHandler rotation)
--     mouseCallback         $= Just (mouseHandler rotation)
--     -- End
-- 
--     mainLoop

{- To update {Particle} position in the time flow -}
clock :: MVar (ExprMaker, [Symbol]) -> MVar [Particle] -> IO ()
clock mvarParser mvarParticles = do
    threadDelay 10000
    (exprX, exprY, exprZ) <- readMVar mvarParser >>= return . expressions . fst
    -- To remove later
    system "clear"
    putStrLn $ "Expression: ("++(mathExpr exprX)++","++(mathExpr exprY)++","++(mathExpr exprZ)++")"
    -- End --
    let f = solve exprX
        g = solve exprY
        h = solve exprZ
        vectorFunction = assign (f,g,h)

    particles    <- takeMVar mvarParticles
    mapM_ (putStrLn . (++) "Particles Before: " . show)  particles
    newParticles <- mapM (react vectorFunction) particles
    mapM_ (putStrLn . (++) "Particles After: " . show)  particles
    putMVar mvarParticles newParticles
    clock mvarParser mvarParticles

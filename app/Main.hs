module Main where

import Graphics.Rendering.OpenGL hiding (bitmap,Program)
import Graphics.UI.GLUT hiding (Text,bitmap,Program)
import Control.Concurrent
import System.IO
import System.Process
import qualified Data.Map as Data.Map
import qualified Data.Maybe as Data.Maybe
import Data.List hiding (singleton)

import Program.Bitmap

import Engine.Object.Particle
import Engine.Math.Dynamics
import Engine.Math.Space
import Engine.Control
import Engine.Render
import qualified Engine.Core as Core

import Compiler.Language.Dictionary
import Compiler.Language.Grammar
import Compiler.Parser
import Compiler.Lexer
import Compiler.Solver

engine = Core.Node
    { Core.program = bitmap
    , Core.caller  = Nothing
    , Core.link    = [] }

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    _window      <- createWindow progName

    engine_ <- newMVar $ engine

    displayCallback       $= addTimerCallback 60 (mainRender engine_)
    keyboardMouseCallback $= Just (mainKeyboardMouse engine_{-MVar Core.Node-})
    mouseCallback         $= Just (mainMouse engine_)
    motionCallback        $= Just (mainMotion engine_)
    -- mouseCallback         $= Just (mouseHandler rotation)
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

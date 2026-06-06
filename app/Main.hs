module Main where

-- import qualified Graphics.Rendering.OpenGL as OpenGL hiding (bitmap,Program)
-- import qualified Graphics.UI.GLUT as GLUT hiding (Text,bitmap,Program)
-- import qualified Control.Concurrent.MVar as MVar
-- import qualified Control.Concurrent as Concurrent
-- import qualified Control.Monad.State as State
-- import qualified Data.Set as Set

-- import qualified Hardware as Hardware
-- import qualified Screen as Screen
-- import qualified Render as Render
-- 
-- import qualified Space as Space
-- import qualified Math as Math
-- 
-- import qualified Program.Magisterium.Magisterium as Magisterium
-- import qualified Program.BitMap.BitMap as BitMap
-- 
-- import qualified System as System
-- import qualified Manager as Manager
-- import qualified Shell as Shell

import qualified Folklore.Lexer as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron as Math.Neuron
import qualified Math.Network as Math.Network

import qualified Manager as Manager

main :: IO ()
main = print $ Main.lex "123.123"

lex :: [Char] -> (Lexer.Token (Lexer.Kind Math.Alphabet.Type),[Char])
lex = Lexer.lex Math.Neuron.start

-- main :: IO ()
-- main = do
--     (progName,_) <- GLUT.getArgsAndInitialize
--     _window      <- GLUT.createWindow progName
--     timerStarter <- Math.getTime            -- normalize: getTime
--     mvarSystem   <- MVar.newMVar $ 
--         System.System (GLUT.Size 300 300)
--             Shell.newShell [] (Manager.fromList ["bitmap","magisterium"]) 
--             $ System.Program 
--                 (Just . BitMap.bitmap    $ GLUT.Size 300 300)
--                 (Just . Magisterium.test $ timerStarter)
-- 
--     Concurrent.forkIO (Hardware.gameTimer mvarSystem)  -- normalize: gameTimer
-- 
--     GLUT.initialWindowSize     GLUT.$= GLUT.Size 300 300
--     GLUT.displayCallback       GLUT.$= GLUT.addTimerCallback 120 (Hardware.display mvarSystem)
-- 
--     GLUT.keyboardMouseCallback GLUT.$= Just (Hardware.keyboardMouse  mvarSystem)
--     GLUT.mouseCallback         GLUT.$= Just (Hardware.mouse          mvarSystem)
--     GLUT.motionCallback        GLUT.$= Just (Hardware.motion         mvarSystem)
--     GLUT.passiveMotionCallback GLUT.$= Just (Hardware.passiveMotion  mvarSystem)
--     GLUT.mainLoop


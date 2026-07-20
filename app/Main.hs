module Main where

import qualified Graphics.Rendering.OpenGL as OpenGL hiding (bitmap,Program)
import qualified Graphics.UI.GLUT as GLUT hiding (Text,bitmap,Program)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.State as State
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Hardware as Hardware
import qualified Screen as Screen
import qualified Render as Render

import qualified Math        as Math
-- import qualified Math.Parser as Math.Parser
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Lexer   as Lexer
import qualified System as System
import qualified Manager as Manager
import qualified Shell as Shell
import qualified Space as Space
import qualified Program.Magisterium.Magisterium as Magisterium
import qualified Program.BitMap.BitMap as BitMap

main :: IO ()
main = do
    (progName,_) <- GLUT.getArgsAndInitialize
    GLUT.initialWindowSize GLUT.$= GLUT.Size 300 300
    _window      <- GLUT.createWindow progName

    time <- Math.getTime

    mvar <- MVar.newMVar
        $ System.System (GLUT.Size 300 300)
            Shell.newShell []
            (Manager.fromList ["bitmap","magisterium"])
            $ System.Program
                (Just . BitMap.bitmap $ GLUT.Size 300 300)
                (Just $ Magisterium.test time)

    tidGameTimer <- Concurrent.forkIO (Hardware.gameTimer mvar)

    MVar.readMVar mvar >>= putStrLn . (++) "Initializing: " . show

    GLUT.displayCallback       GLUT.$= GLUT.addTimerCallback 120 (Hardware.display mvar)
    GLUT.keyboardMouseCallback GLUT.$= Just (Hardware.keyboardMouse  mvar)
    GLUT.mouseCallback         GLUT.$= Just (Hardware.mouse          mvar)
    GLUT.motionCallback        GLUT.$= Just (Hardware.motion         mvar)
    GLUT.passiveMotionCallback GLUT.$= Just (Hardware.passiveMotion  mvar)
    GLUT.mainLoop


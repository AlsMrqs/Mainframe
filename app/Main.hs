module Main where
import qualified Graphics.Rendering.OpenGL as OpenGL hiding (bitmap,Program)
import qualified Graphics.UI.GLUT as GLUT hiding (Text,bitmap,Program)
-- import Csound.IO
-- import Csound.Base
-- import qualified System.Random as Random
import Control.Concurrent.MVar
import Control.Concurrent
-- import qualified Struct.Math     as Math
-- import Struct.Triangle as Triangle
-- import Struct.Space    as Space
-- import Struct.Cube     as Cube
-- import Struct.Render   as Render
-- import Struct.Control  as Control
-- import Struct.Mouse    as Mouse
-- import Struct.Program  as Program
-- import Bitmap as Bitmap
import qualified Data.Set as Set
import qualified Struct.Space as Space
import qualified Struct.Screen as Screen
import qualified Struct.Render as Render
-- import qualified Struct.Control as Control
-- import qualified Struct.Font as Font
-- import qualified Struct.Shell as Shell
import qualified Struct.Shell as Shell
import qualified Struct.Program.BitMap.Callback as BitMap
import qualified Struct.System as System
import qualified Struct.Callback as Callback
import qualified Struct.Manager as Manager
import qualified Struct.Program.Magisterium.Magisterium as Magisterium

main :: IO ()
main = do
    (progName,_) <- GLUT.getArgsAndInitialize
    GLUT.initialWindowSize GLUT.$= GLUT.Size 300 300
    _window      <- GLUT.createWindow progName

    mvar <- newMVar 
        $ System.System (GLUT.Size 300 300)
            Shell.newShell
            (Manager.fromList ["bitmap","magisterium"])
            $ System.Program 
                (Just . BitMap.bitmap $ GLUT.Size 300 300)
                (Just . Magisterium.magisterium $ Magisterium.Ground (5,5))

    readMVar mvar >>= putStrLn . (++) "Initializing: " . show

    GLUT.displayCallback       GLUT.$= GLUT.addTimerCallback 120 (Callback.display mvar)
    GLUT.keyboardMouseCallback GLUT.$= Just (Callback.keyboardMouse  mvar)
    GLUT.mouseCallback         GLUT.$= Just (Callback.mouse          mvar)
    GLUT.motionCallback        GLUT.$= Just (Callback.motion         mvar)
    GLUT.passiveMotionCallback GLUT.$= Just (Callback.passiveMotion  mvar)
    GLUT.mainLoop


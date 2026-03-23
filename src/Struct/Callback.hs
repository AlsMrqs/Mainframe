module Struct.Callback where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Concurrent.MVar as MVar
import qualified Struct.System as System
import qualified Struct.Graph as Graph
-- import qualified Struct.Program.Shell.Callback as Shell
import qualified Struct.Shell as Shell
import qualified Struct.Render as Render
import qualified Struct.Program.BitMap.Callback as BitMapCallback
import qualified Struct.Program.Magisterium.Callback as MagisteriumCallback
import qualified System.Process as Process
import qualified Struct.Math as Math
import qualified Data.List as List
import qualified Control.Monad.State as State

systemStatus :: MVar.MVar System.System -> IO ()
systemStatus mvar = do
    sys <- MVar.readMVar mvar
    Process.system "clear"
    print . System.shell   $ sys
    print . System.manager $ sys
    case System.callDerivative sys of
        Nothing -> return ()
        Just game -> (putStrLn . (++) "Round: " . show . MagisteriumCallback.player) game

display :: MVar.MVar (System.System) -> GLUT.DisplayCallback
display mvar = do
    sys <- (systemStatus mvar >> MVar.readMVar mvar)
    case System.currentProgram sys of

        "bitmap"      -> do
            let display' = BitMapCallback.display
                offline  = "BitMap offline!"
            maybe (putStrLn offline) display' (System.callBitmap sys)

        "derivative"  -> do
            let display' = MagisteriumCallback.display
                offline  = "Derivative offline!"
            maybe (putStrLn offline) display' (System.callDerivative sys)
            
        _             -> return ()

    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvar _key _keyState modifiers _pos = do
    case (modifiers) of
        (GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Down) -> do            -- Alt(Down)
            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> do
                    MVar.modifyMVar_ mvar (return . System.modifier k)
                _                        -> return ()
        (GLUT.Modifiers (GLUT.Up) (GLUT.Up) (GLUT.Up)) -> do        -- Alt(Up)
            systemStatus mvar
            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> do
                    MVar.modifyMVar_ mvar (return . System.shellFunction (Shell.insertInbox k))
                    ---------------------{ Running Game }-------------------
                    if k /= '\r' then return ()
                    else do
                        sys <- MVar.readMVar mvar -- out(MVar)
                        case System.callDerivative sys of
                            Nothing   -> return ()
                            Just game -> do
                                putStrLn "The Game is running!"
                                case (Shell.lastInput . System.shell) sys of
                                    []  -> return ()
                                    str -> do
                                        t0 <- Math.getTime
                                        case State.runStateT (MagisteriumCallback.start t0 str) game of
                                            Left  msg           -> putStrLn msg
                                            Right (msg,newGame) -> MVar.modifyMVar_ mvar (return . System.derivativeFunction (\_ -> newGame))  -- out(MVar)
                    ---------------------------------------------------------
                _                        -> return ()
        _ -> return ()
    case (modifiers,_key,_keyState) of
        (GLUT.Modifiers (GLUT.Down) (GLUT.Up) (GLUT.Up), GLUT.Char k, GLUT.Down) -> do -- Shift(Down)
            systemStatus mvar
            MVar.modifyMVar_ mvar (return . System.shellFunction (Shell.insertInbox k))
        _ -> return ()

mouse :: MVar.MVar (System.System) -> GLUT.MouseCallback
mouse mvar _but _keyState _pos = do
    sys <- MVar.readMVar mvar
    let _size = System.size sys
    case System.currentProgram sys of
        "bitmap" -> do
            MVar.modifyMVar_ mvar $ return  
                . System.bitmapFunction (BitMapCallback.mouse _size _but _keyState _pos)
        _        -> return ()
    return ()

motion :: MVar.MVar (System.System) -> GLUT.MotionCallback
motion mvar _pos = do
    sys <- MVar.readMVar mvar
    case System.currentProgram sys of
        "bitmap" -> return ()
        _        -> return ()
    return ()

passiveMotion :: MVar.MVar (System.System) -> GLUT.MotionCallback
passiveMotion mvar _pos = do
    sys <- MVar.readMVar mvar
    case System.currentProgram sys of
        "bitmap" -> return ()
        _        -> return ()
    return ()


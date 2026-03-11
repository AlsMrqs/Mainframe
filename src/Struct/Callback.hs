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

systemStatus :: MVar.MVar System.System -> IO ()
systemStatus mvar = do
    Process.system "clear"
    print . System.shell   =<< MVar.readMVar mvar
    print . System.manager =<< MVar.readMVar mvar
    putStrLn . (++) "Time: " . show =<< Math.getTime

display :: MVar.MVar (System.System) -> GLUT.DisplayCallback
display mvar = do
    sys <- (systemStatus mvar >> MVar.readMVar mvar)
    case System.currentProgram sys of

        "bitmap"      -> do
            let display' = BitMapCallback.display
                offline  = "BitMap offline!"
            maybe (putStrLn offline) display' (System.callBitmap sys)

        "magisterium" -> do
            let display' = MagisteriumCallback.display
                offline  = "Magisterium offline!"
            maybe (putStrLn offline) display' (System.callMagisterium sys)

        _             -> return ()

    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvar _key _keyState modifiers _pos = do
    case (modifiers) of
        (GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Down) -> do

            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> do
                    MVar.modifyMVar_ mvar (return . System.modifier k)
                _                        -> return ()

        (GLUT.Modifiers (GLUT.Up) (GLUT.Up) (GLUT.Up)) -> do

            systemStatus mvar

            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> do
                    MVar.modifyMVar_ mvar (return . System.shellFunction (Shell.insertInbox k))
                    case k of
                        '\r' -> do
                            sys <- MVar.readMVar mvar
                            if System.currentProgram sys /= "magisterium" then return ()
                            else case System.callMagisterium sys of
                                Nothing     -> return ()
                                Just _magis -> do
                                    let expre     = (Shell.lastInput . System.shell) sys
                                        _newMagis = MagisteriumCallback.insertExpression expre _magis
                                    MVar.modifyMVar_ mvar (return . System.magisteriumFunction (const _newMagis))
                        _    -> return ()
                _                        -> return ()
        _ -> return ()
    
    case (modifiers,_key,_keyState) of
        (GLUT.Modifiers (GLUT.Down) (GLUT.Up) (GLUT.Up), GLUT.Char k, GLUT.Down) -> do

            systemStatus mvar

            MVar.modifyMVar_ mvar (return . System.shellFunction (Shell.insertInbox k))

            case k of
                '\r' -> do
                    sys <- MVar.readMVar mvar
                    if System.currentProgram sys /= "magisterium" then return ()
                    else case System.callMagisterium sys of
                        Nothing     -> return ()
                        Just _magis -> do
                            let expre     = (Shell.lastInput . System.shell) sys
                                _newMagis = MagisteriumCallback.insertExpression expre _magis
                            MVar.modifyMVar_ mvar (return . System.magisteriumFunction (const _newMagis))
                _    -> return ()
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


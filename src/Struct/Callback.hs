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

display :: MVar.MVar (System.System) -> GLUT.DisplayCallback
display mvar = do
    sys <- MVar.readMVar mvar
    case System.currentProgram sys of
        "bitmap" -> do
            maybe (putStrLn "BitMap offline!") BitMapCallback.display 
                $ (System.bitmap . System.program) sys
        _        -> return ()
    return ()
    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

-- altDown   = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Down
-- altUp     = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Up
-- shiftDown = GLUT.Modifiers GLUT.Down GLUT.Up GLUT.Up
-- shiftUp   = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Up

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvar _key _keyState _mod _pos = do
    case (_mod) of
        (GLUT.Modifiers (GLUT.Up) (GLUT.Up) (GLUT.Down)) -> do -- [Alt](Down)

            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> MVar.modifyMVar_ mvar 
                    $ return . System.modifier k
                _                        -> return ()

        (GLUT.Modifiers (GLUT.Up) (GLUT.Up) (GLUT.Up)) -> do -- [Alt](Up)

            case (_key, _keyState) of
                (GLUT.Char k, GLUT.Down) -> MVar.modifyMVar_ mvar 
                    $ return . System.shellFunction (Shell.insertInbox k)
                _                        -> return ()

        _ -> return ()

    (print . System.shell) =<< MVar.readMVar mvar

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


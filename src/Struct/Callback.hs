module Struct.Callback where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Concurrent
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
    print . System.message $ sys
    putStrLn "----------{ Derivative Game }-------------"
    print . System.callDerivative $ sys
    putStrLn . (++) "Time: " . show . MagisteriumCallback.toSecond =<< Math.getTime
    case System.callDerivative sys of
        Nothing -> return ()
        Just game -> do
            let t0 = MagisteriumCallback.time game
            t <- Math.getTime
            print $ MagisteriumCallback.toSecond (t -t0)
    putStrLn "\n\n\n"

gameTimer :: MVar.MVar (System.System) -> GLUT.DisplayCallback
gameTimer mvar = do
    systemStatus mvar
    sys <- MVar.readMVar mvar
    if System.currentProgram sys /= "derivative" then return ()
    else do
        currentTime <- Math.getTime
        case System.callDerivative sys >>= MagisteriumCallback.timeOver currentTime of
            Nothing            -> return ()
            Just (msg,newGame) -> do
                putStrLn msg
                MVar.modifyMVar_ mvar 
                    (return . System.derivativeFunction (const newGame))
    Concurrent.threadDelay 100000
    gameTimer mvar

-----------------------------{ Output }-----------------------------
display :: MVar.MVar (System.System) -> GLUT.DisplayCallback
display mvar = do
    GLUT.clear [GLUT.ColorBuffer]
    -- sys <- (systemStatus mvar >> MVar.readMVar mvar)
    sys <- MVar.readMVar mvar
    case System.currentProgram sys of

        "bitmap"      -> do
            let display' = BitMapCallback.display
                offline  = "BitMap offline!"
            maybe (putStrLn offline) display' (System.callBitmap sys)

        "derivative"  -> do
            case (MagisteriumCallback.readFunction . Shell.lastInput . System.shell) sys of
                Left msg -> return ()
                Right f  -> maybe (pure ()) 
                    (MagisteriumCallback.renderPoints 
                        (MagisteriumCallback.newRGB 0.1 0.1 0.1))
                        (MagisteriumCallback.trace f (0) (200))
            let display' = MagisteriumCallback.display
                offline  = "Derivative offline!"
            maybe (putStrLn offline) display' (System.callDerivative sys)
            
        _ -> return ()

    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

----------------------------------{ Input (Keyboard) }--------------------------------
altUp :: GLUT.Modifiers
altUp = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Up

shiftDown :: GLUT.Modifiers
shiftDown = GLUT.Modifiers GLUT.Down GLUT.Up GLUT.Up

altDownAction :: GLUT.Key -> GLUT.KeyState -> System.System -> System.System
altDownAction key keyState = case (key, keyState) of
    (GLUT.Char x, GLUT.Down) -> System.modifier x
    _                        -> id

type Time = Double 

-- run Game here
altUpAction :: Time -> GLUT.Key -> GLUT.KeyState -> System.System -> System.System
altUpAction t key keyState system = case (key, keyState) of
    (GLUT.Char x, GLUT.Down) -> 
        let update = if x == '\r' 
            then runGameInput t
            else id
        in update (System.shellFunction (Shell.insertInbox x) system)
    _                        -> system

runGameInput :: Time -> System.System -> System.System
runGameInput t system = 
    let input = (Shell.lastInput . System.shell) system in
    case System.callDerivative system of
        Nothing   -> system
        Just game -> do
            case MagisteriumCallback.play t input game of
                Left msg            -> System.insertMessage msg system
                Right (msg,newGame) -> System.derivativeFunction (const newGame) 
                    (System.insertMessage msg system)

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvar key keyState modifiers _pos = do
    t <- Math.getTime
    case modifiers of
        (GLUT.Modifiers _ _ GLUT.Down) -> {- altDown -}
            MVar.modifyMVar_ mvar (return . altDownAction key keyState) 
        altUp     -> MVar.modifyMVar_ mvar (return . altUpAction t key keyState)
        shiftDown -> MVar.modifyMVar_ mvar (return . altUpAction t key keyState)
        _ -> return ()

----------------------------{ Inpub (Mouse) }-------------------------------
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


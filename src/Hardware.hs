module Hardware where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as Concurrent
import qualified System as System
import qualified Graph as Graph
import qualified Shell as Shell
import qualified Render as Render
import qualified Program.BitMap.Display as BitMapDisplay
import qualified Program.Magisterium.Display as MagisteriumDisplay
import qualified Program.Magisterium.Controller as MagisteriumController
import qualified System.Process as Process
import qualified Math as Math
import qualified Data.List as List
import qualified Data.Bool as Bool
import qualified Data.Time.Clock as Clock
import qualified Control.Monad.State as State
-- import qualified Solve as Solve

systemStatus :: MVar.MVar System.System -> IO ()
systemStatus mvar = do
    sys <- MVar.readMVar mvar
    Process.system "clear"
    print . System.shell   $ sys
    print . System.manager $ sys
    print . System.message $ sys
    putStrLn "----------{ Derivative Game }-------------"
    print . System.callMagisterium $ sys
    putStrLn . (++) "Time: " . show . MagisteriumDisplay.toSecond =<< Math.getTime
    case System.callMagisterium sys of
        Nothing -> return ()
        Just game -> do
            let t0 = MagisteriumDisplay.time game
            t <- Math.getTime
            print $ MagisteriumDisplay.toSecond (t -t0)
    putStrLn "\n\n\n"

gameTimer :: MVar.MVar (System.System) -> GLUT.DisplayCallback
gameTimer mvar = do
    systemStatus mvar
    sys <- MVar.readMVar mvar
    if System.currentProgram sys /= "magisterium" then return ()
    else do
        currentTime <- Math.getTime
        randPos     <- MagisteriumController.generateRandomPosition
        case System.callMagisterium sys >>= MagisteriumController.timeOver randPos currentTime of
            Nothing            -> return ()
            Just (msg,newGame) -> do
                putStrLn msg
                MVar.modifyMVar_ mvar (return . System.magisteriumFunction (const newGame))
    Concurrent.threadDelay 100000
    gameTimer mvar

display :: MVar.MVar (System.System) -> GLUT.DisplayCallback
display mvar = do
    GLUT.clear [GLUT.ColorBuffer]
    sys <- MVar.readMVar mvar
    case System.currentProgram sys of

        "bitmap"      -> do
            let display' = BitMapDisplay.display
                offline  = "BitMap offline!"
            maybe (putStrLn offline) display' (System.callBitmap sys)

        "magisterium"  -> do
            case (MagisteriumDisplay.readFunction . Shell.lastInput . System.shell) sys of
                Left msg -> return ()
                Right f  -> maybe (pure ()) 
                    (MagisteriumDisplay.renderPoints 
                        (MagisteriumDisplay.newRGB 0.3 0.3 0.3))
                        (MagisteriumDisplay.trace f (0) (200))
            let display' = MagisteriumDisplay.display
                offline  = "Magisterium offline!"
            maybe (putStrLn offline) display' (System.callMagisterium sys)
            
        _ -> return ()

    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

type Time = Double 

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvarSystem key keyState modifiers position = do
    
    t <- Math.getTime
    case modifiers of
        (GLUT.Modifiers _ _ GLUT.Down) -> MVar.modifyMVar_ mvarSystem
            (pure . altDownAction key keyState) 
        -- shiftDown -> MVar.modifyMVar_ mvarSystem (pure . altUpAction t key keyState)
        -- altUp     -> MVar.modifyMVar_ mvarSystem (pure . altUpAction t key keyState)
        shiftDown -> MVar.modifyMVar_ mvarSystem (altUpAction t key keyState)
        altUp     -> MVar.modifyMVar_ mvarSystem (altUpAction t key keyState)
        _         -> return ()

    where

    altUp :: GLUT.Modifiers
    altUp = GLUT.Modifiers GLUT.Up GLUT.Up GLUT.Up

    shiftDown :: GLUT.Modifiers
    shiftDown = GLUT.Modifiers GLUT.Down GLUT.Up GLUT.Up

    altDownAction :: GLUT.Key -> GLUT.KeyState -> System.System -> System.System
    altDownAction key' keyState' = case (key', keyState') of
        (GLUT.Char x, GLUT.Down) -> System.modifier x
        _                        -> id

    -- altUpAction :: Time -> GLUT.Key -> GLUT.KeyState -> System.System -> System.System
    -- altUpAction t key' keyState' system = case (key', keyState') of
    --     (GLUT.Char x, GLUT.Down) -> Bool.bool id (runGameInput t) ((==) x '\r') 
    --         $ System.shellFunction (Shell.insertInbox x) system
    --     _                        -> system

    altUpAction :: Time -> GLUT.Key -> GLUT.KeyState -> System.System -> IO System.System
    altUpAction t key' keyState' system = case (key', keyState') of
        (GLUT.Char x, GLUT.Down) -> Bool.bool (return . id) (runGameInput t) ((==) x '\r') 
            $ System.shellFunction (Shell.insertInbox x) system
        _                        -> return system

    runGameInput :: Time -> System.System -> IO System.System
    runGameInput t system = maybe (return system) update (System.callMagisterium system)
        where
        input  = (Shell.lastInput . System.shell) system
        update = \game -> do
            result <- MagisteriumDisplay.play t input game 
            case result of
                Left msg            -> return $ System.insertMessage msg system
                Right (msg,newGame) -> return $ System.magisteriumFunction 
                    (const newGame) 
                    (System.insertMessage msg system)

    -- runGameInput :: Time -> System.System -> System.System
    -- runGameInput t system = maybe system update (System.callMagisterium system)
    --     where
    --     input  = (Shell.lastInput . System.shell) system
    --     update = \game -> 
    --         case MagisteriumDisplay.play t input game of
    --             Left msg            -> System.insertMessage msg system
    --             Right (msg,newGame) -> System.magisteriumFunction 
    --                 (const newGame) 
    --                 (System.insertMessage msg system)

mouse :: MVar.MVar (System.System) -> GLUT.MouseCallback
mouse mvar _but _keyState _pos = do
    sys <- MVar.readMVar mvar
    let _size = System.size sys
    case System.currentProgram sys of
        "bitmap" -> do
            MVar.modifyMVar_ mvar $ return  
                . System.bitmapFunction (BitMapDisplay.mouse _size _but _keyState _pos)
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


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
import qualified Data.Bool as Bool
import qualified Control.Monad.State as State

import qualified Solve as Solve -- Convergence

systemStatus :: MVar.MVar System.System -> IO ()
systemStatus mvar = do
    sys <- MVar.readMVar mvar
    Process.system "clear"
    print . System.shell   $ sys
    print . System.manager $ sys
    print . System.message $ sys
    putStrLn "----------{ Derivative Game }-------------"
    -- print . System.callDerivative $ sys
    print . System.callMagisterium $ sys
    putStrLn . (++) "Time: " . show . MagisteriumCallback.toSecond =<< Math.getTime
    -- case System.callDerivative sys of
    case System.callMagisterium sys of
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
    -- if System.currentProgram sys /= "derivative" then return ()
    if System.currentProgram sys /= "magisterium" then return ()
    else do
        currentTime <- Math.getTime
        randPos <- MagisteriumCallback.generateRandomPosition
        -- case System.callDerivative sys >>= MagisteriumCallback.timeOver randPos currentTime of
        case System.callMagisterium sys >>= MagisteriumCallback.timeOver randPos currentTime of
        -- case System.callDerivative sys >>= MagisteriumCallback.timeOver currentTime of
            Nothing            -> return ()
            Just (msg,newGame) -> do
                putStrLn msg
                MVar.modifyMVar_ mvar 
                    -- (return . System.derivativeFunction (const newGame))
                    (return . System.magisteriumFunction (const newGame))
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

        -- "derivative"  -> do
        "magisterium"  -> do
            case (MagisteriumCallback.readFunction . Shell.lastInput . System.shell) sys of
                Left msg -> return ()
                Right f  -> maybe (pure ()) 
                    (MagisteriumCallback.renderPoints 
                        (MagisteriumCallback.newRGB 0.3 0.3 0.3))
                        (MagisteriumCallback.trace f (0) (200))
            let display' = MagisteriumCallback.display
                -- offline  = "Derivative offline!"
                offline  = "Magisterium offline!"
            -- maybe (putStrLn offline) display' (System.callDerivative sys)
            maybe (putStrLn offline) display' (System.callMagisterium sys)
            
        _ -> return ()

    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

----------------------------------{ Input (Keyboard) }--------------------------------
type Time = Double 

keyboardMouse :: MVar.MVar (System.System) -> GLUT.KeyboardMouseCallback
keyboardMouse mvarSystem key keyState modifiers position = do
    
    ----------------- MACHINE ----------------
    sys <- MVar.readMVar mvarSystem
    let positions = case System.callMagisterium sys of
            Nothing   -> Nothing
            Just game -> return $ (,)
                (MagisteriumCallback.tuplePosition $ MagisteriumCallback.positionP1 game)
                (MagisteriumCallback.tuplePosition $ MagisteriumCallback.positionP2 game)

    degree     <- Solve.randomDegree
    expr <- case positions of 
        Nothing      -> return ""
        Just (p1,p2) -> Solve.solution degree p1 p2
    ------------------------------------------
    
    t <- Math.getTime
    case modifiers of
        (GLUT.Modifiers _ _ GLUT.Down) -> MVar.modifyMVar_ mvarSystem -- altDown
            (pure . altDownAction key keyState) 
        shiftDown -> MVar.modifyMVar_ mvarSystem (pure . altUpAction t key keyState)
        altUp     -> MVar.modifyMVar_ mvarSystem (pure . altUpAction t key keyState)
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

    altUpAction :: Time -> GLUT.Key -> GLUT.KeyState -> System.System -> System.System
    altUpAction t key' keyState' system = case (key', keyState') of
        (GLUT.Char x, GLUT.Down) -> 
            let update = Bool.bool id (runGameInput t) ((==) x '\r') 
                in 
                update (System.shellFunction (Shell.insertInbox x) system)
        _                        -> system

    runGameInput :: Time -> System.System -> System.System
    runGameInput t system = 
        let input = (Shell.lastInput . System.shell) system 
            in
            case System.callMagisterium system of
                Nothing   -> system
                Just game -> case MagisteriumCallback.play t input game of
                    Left msg            -> System.insertMessage msg system
                    Right (msg,newGame) -> System.magisteriumFunction 
                        (const newGame) 
                        (System.insertMessage msg system)

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


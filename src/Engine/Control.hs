module Engine.Control  where

import Graphics.UI.GLUT hiding (Cursor, cursor, Text)
import Control.Concurrent
import System.Random
import Data.Map as Map
import Data.List as List
import Data.IORef

import qualified Engine.Core as Core
import qualified Engine.Math.Space as Math

import Compiler.Language.Dictionary
import Compiler.Language.Grammar
import Compiler.Parser
import Compiler.Lexer

-- The real code --
engineKeyboardMouse :: (Window, IORef Int) -> MVar Core.Node -> KeyboardMouseCallback
engineKeyboardMouse (wind, flag) nodeMVar key keyState mod pos = 
    case (key, keyState) of
        (Char '#', Down) -> do
            -- putStrLn "Leaving Engine!"
            -- leaveMainLoop
            putStrLn "Leaving Engine!"
            destroyWindow wind
        (Char '@', Down) -> do
            putStrLn "Starting Bitmap!"
                -- can't work
            -- node <- readMVar nodeMVar
            -- case Core.access "bitmap" node of
            --     Nothing -> putStrLn "Can't find Bitmap!"
            --     Just nd -> do
            --         tid <- forkIO $ (Core.call . Core.program $ nd) Nothing
            --         threadDelay 10000000
            --         killThread tid
            --         return ()
        _ -> return ()

engineMouse :: MVar Core.Node -> MouseCallback
engineMouse nodeMVar button keyState pos = do -- return ()
    putStrLn $ "Mouse Mouse: " ++ show pos

engineMotion :: MVar Core.Node -> MotionCallback
engineMotion nodeMVar pos = do -- return ()
    putStrLn $ "Mouse Motion: " ++ show pos

enginePassiveMotion :: MVar Core.Node -> MotionCallback
enginePassiveMotion nodeMVar pos = do
    node <- readMVar nodeMVar
    let (Position x y) = pos
        x' = (+) (-1) $ (*) 2 (fromIntegral(x)/800)
        y' = (+) (1) $ (*) (-2) (fromIntegral(y)/600)
    putStrLn $ "Mouse Passive Motion: " ++ show (x',y')

    case Map.lookup "Bitmap.Button" (Core.element . Core.program $ node) of
        Nothing -> putStrLn "Can't find Bitmap.Button {Element}"
        Just el -> case uncons $ Core.location el of
            Nothing     -> putStrLn "             In:False"
            Just (x,xs) -> putStrLn 
                $ "Permut:" ++ (show $ f (x',y',0) (shiftPoint x) (List.map shiftPoint xs))

shiftPoint :: (Float, Float, Float) -> Math.Point
shiftPoint (a,b,c) = (realToFrac a, realToFrac b, realToFrac c)
    
f :: Math.Point -> Math.Point -> [Math.Point] -> Bool
f p k []         = False
f p k l@(x:[])   = False
f p k l@(x:y:xs) = Math.isInsideTriangle k x y p || f p k (y:xs)

-- End | The real code --

isInRange :: ([GLfloat],[GLfloat]) -> (GLfloat,GLfloat) -> Bool
isInRange ([a,b],[c,d]) (x,y) = a <= x && x <= b && c <= y && y <= d










{- Mouse Interface -}
data Mouse = Mouse
    { click  :: (Double,Double) 
    , posit  :: (Double,Double) 
    , memory :: (Double,Double)
    } deriving Show

getMemory :: Mouse -> (Double,Double)
getMemory (Mouse (a,b) (x,y) (n,m)) = (((-b+y)/100),((-a+x)/100))

{- Mouse Position x y -}
draggingHandler :: MVar Mouse -> Position -> IO ()
draggingHandler mouseMVar (Position x y) = do
    putStrLn $ "Arrastando mouse em: " ++ show (x, y)

    mouse <- readMVar mouseMVar
    swapMVar mouseMVar 
        $ Mouse (click mouse) (fromIntegral x, fromIntegral y) (getMemory mouse)
    print =<< readMVar mouseMVar
    postRedisplay Nothing
    return ()

{- Mouse {} Button State Position -}
mouseHandler :: MVar Mouse -> MouseButton -> KeyState -> Position -> IO ()
mouseHandler mouseMVar button state pos = do
    putStrLn $ "Mouse Button: " ++ show button
    putStrLn $ "State: "        ++ show state
    putStrLn $ "Position: "     ++ show pos

    let f = (\(Position a b) -> (fromIntegral a,fromIntegral b))
    mouse <- readMVar mouseMVar
    swapMVar mouseMVar $ Mouse (f pos) (f pos) (getMemory mouse)
    print =<< readMVar mouseMVar

{-Test -}

{-Data Structure | to process Input data-}
data Chat = Chat -- CLI
    { activation :: Bool
    , inbox      :: [Char]
    , history    :: [[Char]] } 

turnOn :: Chat -> Chat
turnOn b = b {activation = True}

turnOff :: Chat -> Chat
turnOff b = b {activation = False}

writeInbox :: Char -> Chat -> Chat
writeInbox c b = b {inbox = (inbox b ++ [c])}

updateHistory :: Chat -> Chat
updateHistory b = Chat (activation b) [] $
    if (inbox b) == []
        then history b
        else history b ++ [inbox b]

chatScript :: MVar Chat -> KeyboardMouseCallback
chatScript chatMVar key keyState _ _ = case (key, keyState) of
    (Char '!', Down) -> do
        putStrLn "Saved!"
        chat <- takeMVar chatMVar
        putMVar chatMVar $ updateHistory chat
        readMVar chatMVar >>= putStrLn . (++) "History: " 
            . intercalate "\n" . history
    (Char x, Down) -> do
        chat <- takeMVar chatMVar
        putMVar chatMVar $ writeInbox x chat
        readMVar chatMVar >>= putStrLn . (++) "Inbox: " . inbox
    _ -> return ()

mainKeyboardMouse :: Window -> MVar (Core.Node) -> KeyboardMouseCallback
mainKeyboardMouse wind nodeMVar key keyState motion_ position_ = do
    node <- readMVar nodeMVar 
    ref <- newIORef 1
    Core.keyboardMouse (Core.program node) (wind, ref) nodeMVar key keyState motion_ position_

mainMouse :: MVar (Core.Node) -> MouseButton -> KeyState -> Position -> IO ()
mainMouse nodeMVar button state pos = do
    node <- readMVar nodeMVar
    Core.mouse (Core.program node) nodeMVar button state pos

mainMotion :: MVar (Core.Node) -> Position -> IO ()
mainMotion nodeMVar pos = do
    node <- readMVar nodeMVar
    Core.motion (Core.program node) nodeMVar pos

    -- maybe (exe) is a better name!!!

-- manager :: {-(...) -> -}KeyboardMouseCallback
-- manager key keyState _ _ = case (key, keyState) of

{-End Test-}
    -- case (key, keyState) of
    --     (Char '#', Down) -> leaveMainLoop
    --     (Char x, Down)   -> putStrLn ("Key pressed: "++[x])
    --     _   -> return ()
    
parserKeyboardMouse :: ThreadId -> MVar ([Char], Automaton) -> MVar (ExprMaker, [Symbol]) -> KeyboardMouseCallback
parserKeyboardMouse tid mvarLexer mvarParser key keyState _ _ = case (key,keyState) of
        (Char '#', Down) -> killThread tid >> leaveMainLoop
        (Char x, Down)   -> if x == '#' 
            then leaveMainLoop 
            else runParser x mvarLexer mvarParser
        _                -> return ()

runParser :: Char -> MVar ([Char], Automaton) -> MVar (ExprMaker, [Symbol]) -> IO ()
runParser x mvarLexer mvarParser = do
    contentLexer <- takeMVar mvarLexer
    case lexer' x contentLexer of
        Left  (token, char) -> do
            print token 
            contentParser <- takeMVar mvarParser
            case parser token contentParser of
                Nothing -> putMVar mvarParser contentParser
                Just ct -> print (fst ct) >> putMVar mvarParser ct
            case lexer' char ([], machine) of
                Left  _ -> do
                    putStrLn ("Invalid Input!"++(show char))
                    putMVar mvarLexer contentLexer
                Right s -> putMVar mvarLexer s
        Right (token,char) -> putMVar mvarLexer (token,char)


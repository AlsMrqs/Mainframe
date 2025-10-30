module Engine.Control  where

import Graphics.UI.GLUT hiding (Cursor, cursor, Text)
import Control.Concurrent
import System.Random
import Data.Map
import Data.List

import qualified Engine.Core as Core

import Compiler.Language.Dictionary
import Compiler.Language.Grammar
import Compiler.Parser
import Compiler.Lexer

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
data Chat = Chat
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

mainKeyboardMouse :: MVar (Core.Node) -> KeyboardMouseCallback
mainKeyboardMouse nodeMVar key keyState motion_ position_ = do
    node <- readMVar nodeMVar 
    Core.keyboardMouse (Core.program node) nodeMVar key keyState motion_ position_

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
    
keyboardMouse :: ThreadId -> MVar ([Char], Automaton) -> MVar (ExprMaker, [Symbol]) -> KeyboardMouseCallback
keyboardMouse tid mvarLexer mvarParser key keyState _ _ = case (key,keyState) of
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


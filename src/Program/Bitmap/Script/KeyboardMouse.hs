module Program.Bitmap.Script.KeyboardMouse where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import Control.Concurrent.MVar

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
import Data.List

import Engine.Core

type Err = [Char]
type Msg = [Char]
type Args = [String]

bitmapKeyboardMouse :: (Window, IORef Int) -> MVar Node -> KeyboardMouseCallback
bitmapKeyboardMouse (wind, flag) nodeMVar key keyState mod pos = do
    node <- readMVar nodeMVar
    if active . terminal . program $ node
        then takeMVar nodeMVar >>= terminalCommand mod key keyState >>= putMVar nodeMVar
        else takeMVar nodeMVar >>= graphicalCommand (wind, flag) mod key keyState >>= putMVar nodeMVar

-- command

terminalCommand :: Modifiers -> Key -> KeyState -> Node -> IO Node
terminalCommand mod key keyState = return

graphicalCommand :: (Window, IORef Int) -> Modifiers -> Key -> KeyState -> Node -> IO Node
graphicalCommand (wind, flag) mod key keyState = 
    case (mod, key, keyState) of
        (Modifiers Up Up Up, Char '\ESC', Down) -> closeBitmap (wind, flag)
        (Modifiers Up Up Up, Char '\r', Down) -> activeTerminal
        _ -> return

closeBitmap :: (Window, IORef Int) -> Node -> IO Node
closeBitmap (wind, flag) node = do
    writeIORef flag 0
    putStrLn "Closing Bitmap!"
    destroyWindow wind
    return node

activeTerminal :: Node -> IO Node
activeTerminal node = return $ node {
    program = (program node) { terminal = turnOn . terminal . program $ node }
}

{-
 -
backspaceTerminal :: Node -> Either (Node, Err) (Node, Msg)
backspaceTerminal node = 
    if (not . active . terminal . program $ node)
        then Left $ (node, "Can't erase something!")
        else Right $ (,) 
            (node {
                program = (program node) {
                    terminal = (terminal . program $ node) { 
                        inbox = if null . inbox . terminal . program $ node
                            then inbox . terminal . program $ node
                            else init . inbox . terminal . program $ node
                    }
                }
            })
            ("Erased!")

acceptCharacter :: Char -> Node -> Either (Node, Err) (Node, Msg)
acceptCharacter x node =
                -- not . isAlpha: Rewrite it later!!!!
    if (not . active . terminal . program $ node) -- || (not . isAlpha $ x)
        then Left $ (node, "Can't run command: {"++[x]++"}")
        else Right $ (,)
            (node {
                program = (program node) {
                    terminal = (terminal $ program node) {
                        inbox = (inbox . terminal $ program node) ++ [x]
                    }
                }
            })
            ("Accepted!")

deactiveTerminal :: Node -> Either (Node, Err) (Node, Msg)
deactiveTerminal node = if not . active . terminal . program $ node
    then Left $ (node, "Already off terminal!")
    else Right $ (,)
        (node {
            program = (program node) {
                terminal = turnOff . terminal . program $ node
            }
        })
        ("Terminal turned off!")
    
activeTerminal :: Node -> Either (Node, Err) (Node, Msg)
activeTerminal node = 
    if not . active . terminal . program $ node

        then Right $ (,)
            (node { program = (program node) {terminal = turnOn . terminal . program $ node } })
            ("Terminal Activation: Sucess!!")

        else 
            case (uncons . words . inbox . terminal . program $ node) of
                Nothing              -> 
                    Left $ (node, "No command to execute!")

                Just (command, args) -> 
                    case (Map.lookup command commands) of
                        Nothing -> 
                            Left $ (node, "Unknown command: {"++command++"}")
                        Just f  -> 
                            case (f args (program node)) of
                                Left  (prog_, msg_) -> Left  (node {program = program node}, msg_)
                                Right (prog_, msg_) -> Right (node {program = program node}, msg_)

commands :: Map.Map String  ([String] -> Program -> Either (Program, Err) (Program, Msg))
commands = Map.fromList 
    [ ("saveAs", saveSprite) ]

-- Commands... 
saveSprite :: [String] -> Program -> Either (Program, Err) (Program, Msg)
saveSprite str prog 
    | length str == 1 = Right $ (,)
        ( prog { sprites = Map.insert (concat str) (Set.toList $ polygons prog) (sprites prog) } )
        ( "Saved! | { " ++ concat str ++ " }" )
    | otherwise       = Left $ (,)
        ( prog )
        ( "Out of range {String}:" ++ concat str)
-}

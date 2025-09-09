module Engine.Control 
    ( module Data.Map
    , module Engine.Editor
    , keyboardMouse
    -- , Collector(..)
    -- , Memory(..)
    ) where

import Graphics.UI.GLUT hiding (Cursor, cursor, Text)
import Control.Concurrent
import System.Random
import Data.Map

import Engine.Editor
import Graph.Automaton
import Graph.Grammar
import Lexer
import Parser

-- Control --
keyboardMouse :: MVar ([Char], Automaton) -> MVar (PreVector, [Symbol]) -> KeyboardMouseCallback
keyboardMouse mvarLexer mvarParser key keyState _ _ = do
    case (key,keyState) of
        (Char '#', Down) -> leaveMainLoop
        (Char x, Down)   -> do
            if x == '#'
            then do
                readMVar mvarParser >>= print . fst
                leaveMainLoop 
            else do
                contentLexer <- takeMVar mvarLexer
                case lexer' x contentLexer of
                    Left  (a,b) -> do
                        print a 
                        contentParser <- takeMVar mvarParser
                        case parser a contentParser of
                            Nothing -> putMVar mvarParser contentParser
                            Just ct -> print (fst ct) >> putMVar mvarParser ct
                        case lexer' b ([], machine) of
                            Left  _ -> do
                                putStrLn ("Invalid Input!"++(show b))
                                putMVar mvarLexer contentLexer
                            Right s -> putMVar mvarLexer s
                    Right (a,b) -> putMVar mvarLexer (a,b)
        _                -> return ()


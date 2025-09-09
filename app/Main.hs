module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT hiding (Text)
import Control.Concurrent
import System.IO

import Engine.Control -- (keyboardMouse, state)
import Engine.Render -- (render)
import Graph.Automaton
import Graph.Grammar
import Lexer
import Parser
import Data.List hiding (singleton)

-- Start --
{-
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStrLn "Started!"
    photosynthesis (PreVector X (Empty,Empty,Empty), [Variable grammar])
    return ()
-}

main :: IO ()
main = do
    -- start
    -- hSetBuffering stdin NoBuffering
    -- hSetBuffering stdout NoBuffering
    (progname,_) <- getArgsAndInitialize
    _window      <- createWindow $ progname ++ " - Math Space"

    lexator   <- newMVar ([], machine)
    parseator <- newMVar (PreVector X (Empty,Empty,Empty), [Variable grammar])


    windowSize            $= Size 600 500
    displayCallback       $= addTimerCallback 60 (render parseator)
    keyboardMouseCallback $= Just (keyboardMouse lexator parseator)
    mainLoop

start :: IO ()
start = do
    putStr ">"
    input <- getChar
    if input == '#' 
        then return ()
        else loop $ either (\_ -> Nothing) (Just) $ lexer' input ([], machine)

tokenizer :: ([Char], Automaton) -> IO (Token, Char)
tokenizer state = do
    input <- getChar
    case lexer' input state of
        Left  ansswer -> print (fst ansswer) >> return ansswer
        Right ansswer -> print (fst ansswer) >> tokenizer ansswer

photosynthesis :: (PreVector, [Symbol]) -> IO (PreVector, [Symbol])
photosynthesis state = do
    (token, c) <- tokenizer ([], machine)
    case parser token state of
        Nothing            -> putStrLn ("Invalid Input!"++(show c)) >> return state
        Just (prev, stack) -> print prev >> photoflux c (prev, stack)

photoflux :: Char -> (PreVector, [Symbol]) -> IO (PreVector, [Symbol])
photoflux c state = do
    case lexer' c ([], machine) of
        Left  (token, c1)  -> case parser token state of
                Nothing            -> do 
                    putStrLn ("Invalid Input!"++(show c1))
                    if c1 == c 
                        then photosynthesis state
                        else photoflux c state
                Just (prev, stack) -> print prev >> photoflux c1 (prev, stack)
        Right (str, auto) -> do
            (token2, c2) <- tokenizer (str, auto)
            case parser token2 state of
                Nothing             -> putStrLn ("Invalid Input!"++(show c2)) >> photoflux c2 state
                Just (prev, stack2) -> print prev >> photoflux c2 (prev, stack2)

loop :: Maybe ([Char], Automaton) -> IO ()
loop x = do
    case x of
        Nothing -> do
            putStrLn "Nothing!"
            putStr ">"
            input <- getChar
            loop $ either (\_ -> Nothing) Just $ lexer' input ([], machine)
        Just s  -> do
            putStrLn $ " -> "++(show $ Token (fst s) (typeof $ snd s))
            putStr ">"
            input <- getChar
            let s' = lexer' input s
            case s' of
                Left (tk, c)  -> 
                    loop $ either (\_ -> Nothing) Just $ lexer' input ([], machine)
                Right (cs, a) -> loop $ Just (cs, a) 

-- data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty
--     deriving Show

-- magnitude :: Tree a -> Int
-- magnitude tree = case tree of
--     Node _ l r -> 1 + magnitude l + magnitude r
--     Leaf _     -> 1
--     Empty      -> 0
-- 
-- calc :: Tree Token -> Double -> Double
-- calc tree k = case tree of
--     Node n l r -> case (\(Token x _) -> x) n of
--         "*" -> (calc l k) * (calc r k)
--         "/" -> (calc l k) / (calc r k)
--         "+" -> (calc l k) + (calc r k)
--         "-" -> (calc l k) - (calc r k)
--     Leaf (Token x t) -> case t of
--         Variable_ -> k
--         _         -> (Prelude.read x :: Double)
--     Empty -> k
-- 
-- alloc :: Token -> Tree Token -> Maybe (Tree Token)
-- alloc (Token x t) tree = case tree of
--     Node n l r -> case t of
--         Operator_ -> case r of
--             Empty -> Nothing
--             _     -> if (precedence $ Token x t) > (precedence n) 
--                 then maybe Nothing (Just . Node n l) $ alloc (Token x t) r
--                 else Just $ Node (Token x t) tree Empty
--         _         -> case l of
--             Node _ _ _ -> maybe Nothing (Just . Node n l) $ alloc (Token x t) r
--             Leaf _     -> maybe Nothing (Just . Node n l) $ alloc (Token x t) r
--             Empty      -> Nothing
--     Leaf n     -> case t of
--         Operator_ -> Just $ Node (Token x t) (Leaf n) Empty
--         _         -> Nothing
--     Empty      -> case t of
--         Operator_ -> Nothing
--         _         -> Just $ Leaf (Token x t)
-- 
-- precedence :: Token -> Int
-- precedence (Token x _)
--     | elem x ["*","/"] = 10
--     | elem x ["+","-"] = 5
--     | otherwise        = 0
-- 

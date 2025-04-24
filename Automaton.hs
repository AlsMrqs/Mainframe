module Automaton where

import Data.Maybe
import Data.Bool

import qualified Data.Map as Map

data State = State 
    { final       :: Bool 
    , transitions :: [Transition] } deriving (Show)

data Transition = Transition 
    { alphabet  :: [Char] 
    , nextState :: State } deriving (Show)

signal  = State False [Transition ['+','-']  number0]
number0 = State True  [Transition ['0'..'9'] number1]
number1 = State True  [Transition ['0'..'9'] number1, Transition ['.'] number2] 
number2 = State False [Transition ['0'..'9'] number3] 
number3 = State True  [Transition ['0'..'9'] number3]

step :: State -> Char -> Maybe State
step s c = if null . transitions $ s then Nothing
    else 
    if c `elem` (alphabet x) 
        then Just (nextState x) 
        else step (State (final s) xs) c
    where
        (x:xs) = transitions s

lexer :: [Char] -> State -> ([Char], [Char])
lexer []       _ = ([], [])
lexer l@(x:xs) s = let canRead_x = isJust $ step s x in
    if canRead_x then 
        let state  = fromJust $ step s x
            (a, b) = lexer xs state
        in 
            if null a
                then bool ([], l) (x:[], xs) $ final state
                else (x:a, b)
    else ([], l)
    
main :: IO ()
main = return ()


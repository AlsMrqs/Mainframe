module Automaton where

import Data.Maybe
import Data.Bool

import qualified Data.Map as Map

import Test

data State a = State 
    { isFinal     :: Bool 
    , transitions :: a 
    } deriving (Show) 

instance Functor State where
    fmap f (State b t) = State b (f t)

next :: State [Transition] -> Maybe (State [Transition])
next s | null (transitions s) = Nothing | otherwise = Just (getState . head $ transitions s) 

--next :: State [Transition] -> Maybe (State [Transition])
--next s = if null (transitions s) 
--    then Nothing
--    else Just (getState . head $ transitions s)

data Transition = Transition 
    { alphabet :: [Char] 
    , getState :: State [Transition] 
    } deriving (Show)

signal  = State False [Transition ['+','-']  number0]
number0 = State True  [Transition ['0'..'9'] number1]
number1 = State True  [Transition ['0'..'9'] number1, Transition ['.'] number2] 
number2 = State False [Transition ['0'..'9'] number3] 
number3 = State True  [Transition ['0'..'9'] number3]

step :: Char -> State [Transition] -> Maybe (State [Transition]) 
step character state 
    | null (transitions state) = Nothing
        -- sumarize next line
    | elem character (alphabet . head $ transitions state) = next state 
    | otherwise = step character (fmap tail state) 

lexer :: [Char] -> State [Transition] -> ([Char], [Char])
lexer []       _ = ([], [])
lexer l@(x:xs) state = if isNothing $ step x state
    then ([], l)
    else 
        let nxtState = fromJust $ step x state
            (a, b)   = lexer xs nxtState
        in 
            if null a
                then bool ([], l) (x:[], xs) $ isFinal state
                else (x:a, b)
    
main :: IO ()
main = return ()


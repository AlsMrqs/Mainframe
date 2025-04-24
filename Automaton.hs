module Automaton where

import Data.Maybe
import Data.Bool

import qualified Data.Map as Map

import Test

data State a = State 
    { isFinal     :: Bool 
    , transitions :: a } deriving (Show) -- [Transition] } deriving (Show)

instance Functor State where
    fmap f (State b t) = State b (f t)

popTransition :: State [Transition] -> State [Transition]
popTransition s | null (transitions s) = s | otherwise = fmap tail s

data Transition = Transition 
    { alphabet :: [Char] 
    , getState :: State [Transition] } deriving (Show)

signal  = State False [Transition ['+','-']  number0]
number0 = State True  [Transition ['0'..'9'] number1]
number1 = State True  [Transition ['0'..'9'] number1, Transition ['.'] number2] 
number2 = State False [Transition ['0'..'9'] number3] 
number3 = State True  [Transition ['0'..'9'] number3]

step :: State [Transition] -> Char -> Maybe (State [Transition])
step currentState character = 
    if null (transitions currentState) 
        then Nothing
        else 
            let possibleTransitions@(headTransition:tailTransitions) = transitions currentState 
            in
                if character `elem` (alphabet headTransition)
                    then Just $ getState headTransition
                    else step (popTransition currentState) character

lexer :: [Char] -> State [Transition] -> ([Char], [Char])
lexer []       _ = ([], [])
lexer l@(x:xs) currentState = if isNothing $ step currentState x 
    then ([], l)
    else 
        let state  = fromJust $ step currentState x
            (a, b) = lexer xs state
        in 
            if null a
                then bool ([], l) (x:[], xs) $ isFinal state
                else (x:a, b)
    
main :: IO ()
main = return ()


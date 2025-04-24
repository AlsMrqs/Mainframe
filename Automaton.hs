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

nextState :: State [Transition] -> Maybe (State [Transition])
nextState s | null (transitions s) = Nothing | otherwise = Just (getState . head $ transitions s) 

nextState :: State [Transition] -> Maybe (State [Transition])
nextState s = if null (transitions s) 
    then Nothing
    else Just (getState . head $ transitions s)

data Transition = Transition 
    { alphabet :: [Char] 
    , getState :: State [Transition] 
    } deriving (Show)

signal  = State False [Transition ['+','-']  number0]
number0 = State True  [Transition ['0'..'9'] number1]
number1 = State True  [Transition ['0'..'9'] number1, Transition ['.'] number2] 
number2 = State False [Transition ['0'..'9'] number3] 
number3 = State True  [Transition ['0'..'9'] number3]

--step :: State [Transition] -> Char -> Maybe (State [Transition])
--step currentState character | null (transitions currentState) = Nothing
--    | otherwise = 
--        if character `elem` (alphabet . head $ transitions currentState)
--            then Just $ getState . head $ transitions currentState
--            else step (fmap tail currentState) character

--step :: State [Transition] -> Char -> Maybe (State [Transition]) 
--step currentState character | null (transitions currentState) = Nothing
--step currentState character | elem character $ alphabet . head $ transitions currentState = Just $ getState . head $ transitions currentState
--    | otherwise = step (fmap tail currentState) character

step :: State [Transition] -> Char -> Maybe (State [Transition]) 
step currentState character 
    | null (transitions currentState) = Nothing
        -- This line should be summarized
    | elem character (alphabet . head $ transitions currentState) = nextState currentState -- Just (getState . head $ transitions currentState)
    | otherwise = step (fmap tail currentState) character

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


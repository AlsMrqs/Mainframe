module Graph.Automaton where

import Prelude hiding (read)
import Data.Maybe
import Data.Bool

data State a = State Bool a
data Transition = Transition [Char] (State [Transition])
type Automaton = State [Transition]

instance Functor State where 
    fmap f (State b t) = State b (f t)

alphabet :: Transition -> [Char]
alphabet (Transition x _) = x

getState :: Transition -> State [Transition]
getState (Transition _ x) = x

isFinal :: State [Transition] -> Bool
isFinal (State x _) = x

transitions :: State [Transition] -> [Transition]
transitions (State _ x) = x

next :: State [Transition] -> Maybe (State [Transition])
next state | isLast state = Nothing | otherwise = Just (getState $ nearby state)

isLast :: State [Transition] -> Bool
isLast = null . transitions

nearby :: State [Transition] -> Transition
nearby = head . transitions

step :: Char -> State [Transition] -> Maybe (State [Transition]) 
step x state 
    | isLast state = Nothing
    | elem x . alphabet $ nearby state = next state 
    | otherwise = step x (fmap tail state) 

accept :: Char -> State [Transition] -> Bool
accept x = isJust . step x

read :: State [Transition] -> [Char] -> ([Char], [Char])
read _     [] = ([], [])
read state l@(x:xs)
    | isNothing (step x state) = ([], l) 
    | otherwise = 
        if token == []
            then bool ([], l) (x:[], xs) . isFinal $ fromJust (step x state)
            else (x:token, rest)
    where
        (token, rest) = read (fromJust $ step x state) xs

readable :: State [Transition] -> [Char] -> Bool
readable = (\state -> (==) "" . snd . read state)


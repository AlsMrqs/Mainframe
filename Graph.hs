module Graph where

import Prelude hiding (read)
import Data.Maybe
import Data.Bool

data State a = State Bool a deriving Show

instance Functor State where 
    fmap f (State b t) = State b (f t)

data Transition = Transition
    { alphabet :: [Char]
    , getState :: State [Transition] } deriving Show

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
step character state 
    | isLast state = Nothing
    | elem character . alphabet $ nearby state = next state 
    | otherwise = step character (fmap tail state) 

--readable :: Char -> State [Transition] -> Bool
--readable = isJust . step

read :: [Char] -> State [Transition] -> ([Char], [Char])
read []       _ = ([], [])
read l@(x:xs) state 
    | isNothing (step x state) = ([], l) 
    | otherwise = 
        if token == []
            then bool ([], l) (x:[], xs) . isFinal $ fromJust (step x state)
            else (x:token, rest)
    where
        (token, rest) = read xs . fromJust $ step x state


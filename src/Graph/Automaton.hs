module Graph.Automaton where

import Prelude hiding (read)
import Data.Maybe
import Data.Bool

data Transition = Transition [Char] (State [Transition]) deriving Show
data State a = State Type Bool a deriving Show

data Type = Starter_ | Separator_ | Finisher_
    | Punctuation_ 
    | Operator_ | Variable_ | Integer_ | Double_ | None_ deriving (Show, Eq)

data Token = Token [Char] Type deriving Show

type Automaton = State [Transition]

instance Functor State where 
    fmap f (State type_ prep content) = State type_ prep (f content)

-- Deprecated -- 
typeof :: Automaton -> Type
typeof (State x _ _) = x

alphabet :: Transition -> [Char]
alphabet (Transition x _) = x

getState :: Transition -> State [Transition]
getState (Transition _ x) = x

isFinal :: State [Transition] -> Bool
isFinal (State _ x _) = x

transitions :: State [Transition] -> [Transition]
transitions (State _ _ x) = x

next :: State [Transition] -> Maybe (State [Transition])
next state 
    | isLast state = Nothing 
    | otherwise    = Just $ getState (nearby state)

isLast :: State [Transition] -> Bool
isLast = null . transitions

nearby :: State [Transition] -> Transition
nearby = head . transitions

step :: Char -> State [Transition] -> Maybe (State [Transition]) 
step x state 
    | isLast state                     = Nothing
    | elem x . alphabet $ nearby state = next state
    | otherwise                        = step x (fmap tail state) 

accept :: Char -> State [Transition] -> Bool
accept x = isJust . step x

-- turn into readable! and try rename!
read :: State [Transition] -> [Char] -> ([Char], [Char])
read _     []       = ([], [])
read state l@(x:xs) = case step x state of
    Nothing        -> ([], l)
    Just nextState -> 
        let (token, rest) = read nextState xs in 
        if token == [] 
            then bool ([], l) (x:[], xs) $ isFinal nextState
            else (x:token, rest)

readable :: State [Transition] -> [Char] -> Bool
readable state = (==) "" . snd . read state


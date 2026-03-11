module Struct.Compiler.Lexer where

import Prelude hiding (exponent, read, exp)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (find)
import Data.Bool

data Type = Starter_ 
          | Separator_ 
          | Finisher_ 
          | Punctuation_ 
          | Operator_ 
          | Variable_ 
          | Integer_ 
          | Double_ 
          | None_ 
          | Funct_
    deriving (Show, Eq)

data Token = Token [Char] Type 
    -- deriving Show

instance Show Token where
    show (Token str kind) = "Token '" ++ str ++ "' " ++ (show kind)

data State a = State Type Bool a 
    deriving Show

data Transition = Transition [Char] (State [Transition]) 
    deriving Show

instance Functor State where 
    fmap f (State type_ prep content) = State type_ prep (f content)

type Automaton = State [Transition]

tokentype :: Token -> Type
tokentype (Token _ x) = x

typeof :: Automaton -> Type
typeof (State x _ _) = x

content :: Token -> [Char]
content (Token x _) = x

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

read :: State [Transition] -> [Char] -> ([Char], [Char])
read _     []       = ([], [])
read state l@(x:xs) = maybe ([],l) loop (step x state)
    where
    loop newState = let (token, rest) = read newState xs in
        if token == [] 
            then bool ([],l) (x:[],xs) (isFinal newState)
            else (x:token, rest)

readable :: State [Transition] -> [Char] -> Bool
readable state = (==) "" . snd . read state

cross :: Char -> Automaton -> (Bool, Automaton)
cross c auto = maybe (False,auto) ((,) True) (step c auto)

lexer' :: Char -> ([Char], Automaton) -> Either (Token, Char) ([Char], Automaton)
lexer' x (str,auto) = result
    where
    (accepted,nextAuto) = cross x auto
    result
        | accepted  = Right $ (,) ((++) str [x]) nextAuto
        | otherwise = Left  $ (,) (Token str (typeof auto)) x


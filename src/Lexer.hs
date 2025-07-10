module Lexer where

import Prelude hiding (exponent, read, exp)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (find)
import Graph.Automaton

num = ['0'..'9']
sig = ['+','-']
idt = ['x','y','z','X','Y','Z']
idT = '_' : (['a'..'z'] ++ ['A'..'Z'])
ope = ['+','-','*','/','^']
exp = ['e','E']
pnt = ['.'] 
pun = ['.','(',',',')']

identifier = State False [Transition (idt) final]

operator = State False [ Transition (ope) final ]
punctuation = State False [ Transition (pun) final ]
final = State True []

number = State False [ Transition (num) integer ]
integer = State True  [ Transition (num) integer, Transition (pnt) point, Transition (exp) exponent ] 
point = State False [ Transition (num) double ] 
double = State True  [ Transition (num) double, Transition (exp) exponent ]
exponent = State False [ Transition (sig) expoSignal, Transition (num) expoInteger ]
expoSignal = State False [ Transition (num) expoInteger ]
expoInteger = State True  [ Transition (num) expoInteger ]

lexer :: [Char] -> ([Char], [Char])
lexer [] = ([], [])
lexer (' ':xs) = lexer xs
lexer l@(x:xs) 
    | isNothing automaton = ([], l) 
    | otherwise = read (fromJust automaton) l
    where
        automaton = find (accept x)
            $ [identifier, operator, number, punctuation]


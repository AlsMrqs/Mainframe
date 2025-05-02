module Automaton where

import Prelude hiding (exponent, read, exp)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (find)
import Graph

num = ['0'..'9']
sig = ['+','-']
idt = '_' : ['a'..'z']
idT = '_' : (['a'..'z'] ++ ['A'..'Z'])
ope = ['+','-','*','/','^']
exp = ['e','E']
pnt = ['.','(',',',')']

identifier = State False [Transition (idt) _identifier]
_identifier = State True [Transition (idT) _identifier]

operator = State False [ Transition (ope) final ]
punctuation = State False [ Transition (pnt) final ]
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
lexer l@(x:xs) 
    | isNothing automaton = ([], l) 
    | otherwise = read (fromJust automaton) l
    where
        automaton = find (accept x) -- Language.Automaton ->
            $ [identifier, operator, number, punctuation]


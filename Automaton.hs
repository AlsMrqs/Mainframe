module Automaton where

import Prelude hiding (exponent)
import Data.List (find)
import Graph

identifier = State False [Transition ('_' : ['a'..'z']) _identifier]
_identifier = State True [Transition (('_' : ['a'..'z']) ++ ['A'..'Z'] ++ ['0'..'9']) _identifier]

signal = State False [Transition ['+','-','*','/','^','='] final]

number = State False [Transition ['0'..'9'] integer]
integer = State True  [Transition ['0'..'9'] integer, Transition ['.'] point, Transition ['e'] exponent] 
point = State False [Transition ['0'..'9'] double] 
double = State True  [Transition ['0'..'9'] double, Transition ['e'] exponent]
exponent = State False [Transition ['+','-'] expoSignal, Transition ['0'..'9'] expoInteger]
expoSignal = State False [Transition ['0'..'9'] expoInteger]
expoInteger = State True  [Transition ['0'..'9'] expoInteger]

final = State True []

start :: Char -> Maybe (State [Transition])
start x = find (elem x . alphabet . nearby) [identifier, signal, number]


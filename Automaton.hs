module Automaton where

import Prelude hiding (exponent)
import Graph

signedNumber = State False [Transition ['+','-'] signal]
signal = State True  [Transition ['0'..'9'] integer]
integer = State True  [Transition ['0'..'9'] integer, Transition ['.'] point, Transition ['e'] exponent] 
point = State False [Transition ['0'..'9'] double] 
double = State True  [Transition ['0'..'9'] double, Transition ['e'] exponent]
exponent = State False [Transition ['+','-'] expoSignal, Transition ['0'..'9'] expoInteger]
expoSignal = State False [Transition ['0'..'9'] expoInteger]
expoInteger = State True  [Transition ['0'..'9'] expoInteger]


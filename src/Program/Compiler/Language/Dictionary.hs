module Compiler.Language.Dictionary where

import Prelude hiding (read,exponent,exp)
import Data.Maybe
import Data.Bool

import Compiler.Lexer

num = ['0'..'9']
sig = ['+','-']
idt = ['x','y','z','X','Y','Z']
idT = '_' : (['a'..'z'] ++ ['A'..'Z'])
ope = ['+','-','*','/','^']
exp = ['e','E']
pnt = ['.'] 
pun = ['.','(',',',')']

open      = State None_ False [Transition ['('] $ State Starter_ True []]
close     = State None_ False [Transition [')'] $ State Finisher_ True []]
separator = State None_ False [Transition [','] $ State Separator_ True []]

machine = State None_ False 
    [ Transition (idt) identifier
    , Transition (ope) operator
    , Transition (num) integer 
    , Transition (pun) punctuation ]

punctuation = State Punctuation_ True []

preNumber     = State None_ False [ Transition (num) integer ]
preIdentifier = State None_ False [ Transition (idt) identifier ]
preOperator   = State None_ False [ Transition (ope) operator ]

identifier  = State Variable_ True []
operator    = State Operator_ True []

integer           = State Integer_ True [ Transition (num) integer, Transition (pnt) point, Transition (exp) exponentInteger ] 
exponentInteger   = State None_ False [ Transition (sig) expoSignalInteger, Transition (num) expoInteger ]
expoSignalInteger = State None_ False [ Transition (num) expoInteger ]
expoInteger       = State Integer_ True  [ Transition (num) expoInteger ]

point              = State None_ False [ Transition (num) double ] 

double             = State Double_ True  [ Transition (num) double, Transition (exp) exponentDouble ]
exponentDouble     = State None_ False [ Transition (sig) expoSignalDouble, Transition (num) expoDouble ]
expoSignalDouble   = State None_ False [ Transition (num) expoDouble ]
expoDouble         = State Double_ True  [ Transition (num) expoDouble ]

final x = State x True []


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

lexer :: [Char] -> ([Char], [Char])
lexer []       = ([], [])
lexer (' ':xs) = lexer xs
lexer l@(x:xs) = case lexer' x ([], machine) of
    Left  ((Token x _), _) -> (x, l)
    Right s'               -> middle xs s'

middle :: [Char] -> ([Char], Automaton) -> ([Char], [Char])
middle []       s = (fst s, [])
middle (' ':xs) s = middle xs s
middle l@(x:xs) s = case lexer' x s of
    Left  ((Token y _), _) -> (y, l)
    Right s'               -> middle xs s'

f1 :: Char -> Automaton -> (Bool, Automaton)
f1 = \c s -> maybe (False, s) (\x -> (True,x)) $ step c s

lexer' :: Char -> ([Char], Automaton) -> Either (Token, Char) ([Char], Automaton)
lexer' c (str, s) = let (b, s') = f1 c s in 
    if b 
        then Right (str ++ [c], s') 
        else Left  (Token str $ typeof s, c)


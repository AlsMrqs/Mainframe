module Parser where

import Data.Either
import Data.Maybe
import Data.List
import Graph.Automaton
import Graph.Grammar
import Lexer

open = State False [Transition ['('] $ State True []]
close = State False [Transition [')'] $ State True []]
separator = State False [Transition [','] $ State True []]

grammar = [ Production [ Terminal open
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal close ] ]

expression = 
    [ Production [Terminal number, Variable expansion]
    , Production [Terminal identifier, Variable expansion] ]
    --, Production [Terminal open, Variable expression, Terminal close, Variable expansion] ]
expansion = [ Production [Terminal operator, Variable expression]
            , Production [] ]

type Error = [Char]

parser :: [Char] -> Either Error [Char]
parser []    = Left "Empty"
parser input = 
    if isNothing production 
        then Left ("GLC can't read: " ++ token)
        else 
            if isRight . manager input $ chain (fromJust production) 
                then Right input
                else Left ("Parser.parser: manager - " ++ input)
    where
        (token, rest) = lexer input 
        production = find (match token) grammar

    -- manager :: (...) -> Stack -> (...)
manager :: [Char] -> [Symbol] -> Either Error Bool
manager []    []    = Right True
manager []    stack = Left ("Input: 0 -|- Stack: " ++ (show $ length stack))
manager input []    = Left ("Input: " ++ (show $ length input) ++ "-|- Stack: 0")
manager input stack = 
    let (token, rest) = lexer input
        (symbol: _) = stack
    in if token == "" 
        then Left ("Parser.manager: lexer - " ++ [head rest]) 
        else if isNothing (resolve token symbol)
            then Left ("Parser.manager: resolve - " ++ token)
            else 
                let (tok, prod) = fromJust (resolve token symbol) 
                in  manager (tok ++ rest) (prod ++ tail stack)


module Parser where

import Data.Maybe
import Data.Tree
import Data.List
import Graph.Automaton
import Graph.Grammar
import Lexer

open = State False [Transition ['('] $ State True []]
separator = State False [Transition [','] $ State True []]
close = State False [Transition [')'] $ State True []]

grammar = [ Production [Terminal open, Variable element, Terminal close] ]
element = [ Production [Variable expression, Variable composition] ]
composition = [ Production [Terminal separator, Variable element]
              , Production [] ]

expression = 
    [ Production [Terminal number, Variable expansion]
    , Production [Terminal identifier, Variable expansion] 
    , Production [Terminal open, Variable expression, Terminal close, Variable expansion] ]
expansion = [ Production [Terminal operator, Variable expression]
            , Production [] ]

type Error = [Char]

parser :: [Char] -> Either Error Bool
parser []    = Left "Empty"
parser input =
    let (token, rest) = lexer input 
        production = find (match token) (grammar :: [Production]) in
    if isNothing production
        then Left ("GLC can't read: " ++ token)
        else manager input (chain $ fromJust production) 

manager :: [Char] -> [Symbol] -> Either Error Bool
manager []    []    = Right True
manager []    stack = Left ("Input: 0 -|- Stack: " ++ (show $ length stack))
manager input []    = Left ("Input: " ++ (show $ length input) ++ "-|- Stack: 0")
manager input stack 
    | token == "" = Left ("manager.lexer: " ++ [head rest]) 
    | otherwise = case symbol of
        (Terminal term) -> if readable term token then manager rest xs else Left ("Can't read: " ++ token)
        (Variable rule) -> if isJust (find (match token) rule)
            then manager input ((chain $ fromJust (find (match token) rule)) ++ xs)
            else  
                if all (isVariable . head . chain) rule 
                    then manager input ((chain $ head rule) ++ xs)
                    else 
                        if any (null . chain) rule
                            then manager input xs
                            else Left ("Don't match: " ++ (token++rest) ++ "\n Stack: " ++ (show $ length stack))
    where
        (symbol:xs) = stack
        (token, rest) = lexer input


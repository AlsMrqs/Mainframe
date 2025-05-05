module Parser where

import Data.Maybe
import Data.List
import Graph.Automaton
import Graph.Grammar
import Lexer

open = State False [Transition ['('] $ State True []]
separator = State False [Transition [','] $ State True []]
close = State False [Transition [')'] $ State True []]

grammar = [ Production [ Terminal open
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal close ] ]

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
    if isNothing production 
        then Left ("GLC can't read: " ++ token)
        else manager input $ chain (fromJust production) 
    where
        (token, rest) = lexer input 
        production = find (match token) grammar

    -- manager :: (...) -> Stack -> (...)
manager :: [Char] -> [Symbol] -> Either Error Bool
manager []    []    = Right True
manager []    stack = Left ("Input: 0 -|- Stack: " ++ (show $ length stack))
manager input []    = Left ("Input: " ++ (show $ length input) ++ "-|- Stack: 0")
manager input stack = 
    if token == "" 
        then Left ("manager.lexer: " ++ [head rest]) 
        else if isNothing (stacker token stack)
            then Left ("manager.stacker: " ++ token)
            else 
                let (tok, prod) = fromJust (stacker token stack) 
                in  manager (tok ++ rest) (prod ++ tail stack)
    where
        (token, rest) = lexer input

    -- stacker :: Token -> Stack -> Maybe (Token, Production)
stacker :: [Char] -> [Symbol] -> Maybe ([Char], [Symbol]) 
stacker []    stack = Nothing 
stacker token stack = case symbol of
    (Terminal term) -> if readable term token then Just ([], []) else Nothing
    (Variable rule) -> 
        if isJust (find (match token) rule) 
            then Just $ (token, chain $ fromJust (find (match token) rule))
        else if all (isVariable . head . chain) rule 
            then Just $ (token, chain $ head rule)
        else if any (null . chain) rule 
            then Just (token, [])
        else Nothing
    where
        (symbol : xs) = stack


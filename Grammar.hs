module Grammar where

import Data.Maybe
import Data.Tree
import Data.List
import Automaton -- (lexer)
import Graph

type Automaton = State [Transition]

data Symbol = Variable [Production] | Terminal Automaton

newtype Production = Production { chain :: [Symbol] }

isVariable :: Symbol -> Bool
isVariable x = case x of
    (Variable _) -> True
    _            -> False

isTerminal :: Symbol -> Bool
isTerminal = not . isVariable

fromTerminal :: Symbol -> Automaton
fromTerminal (Terminal x) = x
fromTerminal _            = error "Symbol.fromTerminal: Variable"

fromVariable :: Symbol -> [Production]
fromVariable (Variable x) = x
fromVariable _            = error "Symbol.fromVariable: Terminal"

open = State False [Transition ['('] $ State True []]
separator = State False [Transition [','] $ State True []]
close = State False [Transition [')'] $ State True []]

source = [ Production [Terminal open, Variable element, Terminal close] ] -- [ tuple ]
element = [ Production [Variable expression, Variable composition] ]

composition = [ Production [Terminal separator, Variable element], Production [] ]
expression = 
    [ Production [Terminal identifier, Variable expansion] 
    , Production [Terminal number, Variable expansion]
    , Production [Terminal open, Variable expression, Terminal close, Variable expansion] ]
expansion = [ Production [Terminal operator, Variable expression], Production [] ]

type Token = [Char]
type Rule = [Production]

match :: Token -> Production -> Bool
match token prod
    | null . chain $ prod = False
    | isVariable . head . chain $ prod = True
    | otherwise = equal (fromTerminal . head $ chain prod) token
    where
        equal = readable

--select :: Token -> [Production] -> Maybe Production
--find (match token) = find (match token)

type Error = [Char]

parser :: [Char] -> Either Error (Tree Token)
parser input =
    let (token, rest) = lexer input
        rule = (find (match token) source :: Maybe Production) 
    in
        if isNothing rule
            then Left ("Grammar.parser: match - " ++ token)
            else manager input (chain $ fromJust rule) (Node [] [])

type Stack = [Symbol]

manager :: [Char] -> [Symbol] -> Tree [Char] -> Either Error (Tree Token)
manager []    []    tree = Right tree
manager []    stack _    = Left ("Grammar.manager: [Symbol] - " ++ (show $ length stack))
--                       ^ Incomplete GLC (input)
manager input stack tree
    | token == "" = Left ("Grammar.manager: lexer - " ++ [head input])
    | otherwise = 

        let (symbol:xs) = stack in case symbol of
            (Terminal term) -> if equal term token
                then manager rest xs tree 
                else Left ("Grammar.manager: stack.readable - " ++ token)
            (Variable var) -> if isNothing (find (match token) var)
                then Left ("Grammar.manager: stack.select - " ++ token)
                else manager input ((chain . fromJust $ find (match token) var) ++ stack) tree
            
    where
        (token, rest) = lexer input
        equal = readable

understand :: [Char] -> Symbol -> Bool
understand token s = case s of
    (Variable prod) -> True
    (Terminal auto) -> readable auto token



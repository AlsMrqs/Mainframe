module Graph.Grammar where

import Data.Maybe
import Data.Tree
import Data.List
import Graph.Automaton

data Symbol = Variable [Production] | Terminal Automaton 
    deriving Show -- temp
newtype Production = Production { chain :: [Symbol] }
    deriving Show

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

type Token = [Char]

match :: Token -> Production -> Bool
match token prod
    | null . chain $ prod = False
    | isVariable . head . chain $ prod = False
    | otherwise = equal (fromTerminal . head $ chain prod) token
    where
        equal = readable

resolve:: Token -> Symbol -> Maybe (Token, [Symbol])
resolve token symbol = case symbol of
    (Terminal term) -> if readable term token
        then Just ([], [])
        else Nothing
    (Variable rule) -> 
        let production = find (match token) rule 
            result
                | isJust production = fmap ((,) token . chain) production
                | all (isVariable . head . chain) rule = Just $ ((,) token . (chain . head)) rule
                | any (null . chain) rule = Just (token, [])
                | otherwise = Nothing
        in result


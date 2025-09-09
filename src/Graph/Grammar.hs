module Graph.Grammar where

import Data.Maybe
import Data.Tree
import Data.List
import Graph.Automaton

newtype Production = Production { chain :: [Symbol] } deriving Show

data Symbol = Variable [Production] | Terminal Automaton deriving Show

type Grammar = [Symbol]

-- New Code --



-- Deprecated --
isVariable :: Symbol -> Bool
isVariable x = case x of
    (Variable _) -> True
    _            -> False

isTerminal :: Symbol -> Bool
isTerminal = not . isVariable

fromTerm :: Symbol -> Maybe Automaton
fromTerm x = case x of
    Terminal t -> Just t
    _          -> Nothing

fromVar :: Symbol -> Maybe [Production]
fromVar x = case x of
    Variable v -> Just v
    _          -> Nothing

type Chain = [Char]

resolve :: Chain -> Symbol -> Maybe (Chain, [Symbol])
resolve token symbol = case symbol of
    Terminal auto -> if readable auto token then Just ([], []) else Nothing
    Variable rule -> 
        let production = find (match token) rule 
            result
                | isJust production       = fmap ((,) token . chain) production
                | all (isVariable . head . chain) rule 
                                          = Just $ ((,) token . (chain . head)) rule
                | any (null . chain) rule = Just (token, [])
                | otherwise               = Nothing
        in result

match :: Chain -> Production -> Bool
match token prod 
    | null $ chain prod = False 
    | otherwise         = case head $ chain prod of
        Variable _    -> False
        Terminal auto -> readable auto token

-- Deprecated --
fromTerminal :: Symbol -> Automaton
fromTerminal (Terminal x) = x
fromTerminal _            = error "Symbol.fromTerminal: Variable"

fromVariable :: Symbol -> [Production]
fromVariable (Variable x) = x
fromVariable _            = error "Symbol.fromVariable: Terminal"
-- End


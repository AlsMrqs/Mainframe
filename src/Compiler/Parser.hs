module Compiler.Parser where

import Data.Either
import Data.Maybe
import Data.List

import Engine.Math.Space
import Compiler.Lexer

type Error = [Char]

-- Grammar -- 
type Expression = Tree Token

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty deriving (Eq, Show)

data Axis = X | Y | Z deriving (Show, Enum)

data ExprMaker = ExprMaker 
    { axis        :: Axis
    , expressions :: (Expression, Expression, Expression) } 

instance Show (ExprMaker) where
    show (ExprMaker fl (a,b,c)) = show a ++ 
        "\n" ++ show b ++ 
        "\n" ++ show c ++ "\n"
-- End -- 

newtype Production = Production { chain :: [Symbol] } deriving Show

data Symbol = Variable [Production] | Terminal Automaton deriving Show

type Grammar = [Symbol]

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

-- Parser
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

-- Parser
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
-- End -- 

-- Grammar
mathExpr :: Tree Token -> [Char]
mathExpr tree = case tree of
    Node (Token x _) l r -> mathExpr l ++ x ++ mathExpr r
    Leaf (Token x _)     -> x
    Empty                -> ""
  
-- Grammar
setAxis :: Axis -> ExprMaker -> ExprMaker
setAxis k exprMaker = ExprMaker k $ expressions exprMaker

-- data Parser = (Ast, Grammar) - {  Grammar :: [Symbol] }
-- parser :: Token -> Parser -> Either (Token, Error) Parser
parser :: Token -> (ExprMaker, [Symbol]) -> Maybe (ExprMaker, [Symbol])
parser (Token x t) (pvec, stack) = if x == "" then Nothing 
    else case check (Token x t) stack of
        (True, rest)  -> let (a,b,c) = expressions pvec in 
            if elem t [Operator_, Integer_, Double_, Variable_] 
            then case axis pvec of
                X -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (n,b,c), rest)) 
                    $ alloc (Token x t) a 
                Y -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,n,c), rest)) 
                    $ alloc (Token x t) b
                Z -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,b,n), rest)) 
                    $ alloc (Token x t) c
            else Just (ExprMaker (if x == "," then succ $ axis pvec else axis pvec) (a,b,c), rest)
        (False, rest) -> Nothing

-- Parser 
check :: Token -> [Symbol]-> (Bool, [Symbol])
check (Token x t) ls = case uncons ls of
    Nothing           -> (False, []) 
    Just (smbl, rest) -> case smbl of
        Terminal auto -> if readable auto x then (True, rest) else (False, ls)
        Variable rule -> case find (match x) rule of
            Nothing   -> if any (null . chain) rule 
                then check (Token x t) rest -- (False, ls)
                else (False, ls)
            Just prod -> case uncons $ chain prod of
                Nothing         -> (True, rest)
                Just (_, prod_) -> (True, prod_ ++ rest)

-- Grammar
magnitude :: Tree a -> Int
magnitude tree = case tree of
    Node _ l r -> 1 + magnitude l + magnitude r
    Leaf _     -> 1
    Empty      -> 0

-- Grammar
alloc :: Token -> Tree Token -> Maybe (Tree Token)
alloc (Token x t) tree = case tree of
    Node n l r -> case t of
        Operator_ -> case r of
            Empty -> Nothing
            _     -> if (precedence $ Token x t) > (precedence n) 
                then maybe Nothing (Just . Node n l) $ alloc (Token x t) r
                else Just $ Node (Token x t) tree Empty
        _         -> case l of
            Node _ _ _ -> maybe Nothing (Just . Node n l) $ alloc (Token x t) r
            Leaf _     -> maybe Nothing (Just . Node n l) $ alloc (Token x t) r
            Empty      -> Nothing
    Leaf n     -> case t of
        Operator_ -> Just $ Node (Token x t) (Leaf n) Empty
        _         -> Nothing
    Empty      -> case t of
        Operator_ -> Nothing
        _         -> Just $ Leaf (Token x t)

-- Grammar
precedence :: Token -> Int
precedence (Token x _)
    | elem x ["*","/"] = 10
    | elem x ["+","-"] = 5
    | otherwise        = 0


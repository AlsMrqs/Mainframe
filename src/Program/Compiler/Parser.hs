module Compiler.Parser where

import Data.Either
import qualified Data.Maybe as Maybe 
import qualified Data.List as List
import qualified Data.Bool as Bool (bool)

import Engine.Math.Space
import qualified Compiler.Lexer as Lexer

-- Grammar -- 
data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty 
    deriving (Eq, Show)

data Axis = X | Y | Z 
    deriving (Show, Enum)

type Expression = Tree Lexer.Token -- HERE!!

data ExprMaker = ExprMaker 
    { axis        :: Axis
    , expressions :: (Expression, Expression, Expression) } 

instance Show (ExprMaker) where
    show (ExprMaker fl (a,b,c)) = show a ++ 
        "\n" ++ show b ++ 
        "\n" ++ show c ++ "\n"
-- End -- 

newtype Production = Production { chain :: [Symbol] } 
    deriving Show

data Symbol = Variable [Production] | Terminal Lexer.Automaton 
    deriving Show

-- Deprecated --
isVariable :: Symbol -> Bool
isVariable (Variable _) = True
isVariable _            = False

isTerminal :: Symbol -> Bool
isTerminal = not . isVariable

fromTerm :: Symbol -> Maybe Lexer.Automaton
fromTerm (Terminal t) = Just t
fromTerm _            = Nothing

fromVar :: Symbol -> Maybe [Production]
fromVar (Variable v) = Just v
fromVar _            = Nothing

resolve :: [Char] -> Symbol -> Maybe ([Char], [Symbol])
resolve str (Terminal auto) = Bool.bool Nothing (Just ([],[])) (Lexer.readable auto str)
resolve str (Variable rule) = result
    where
    production = List.find (match str) rule -- only (Terminal auto)
    result
        | Maybe.isJust production              = fmap ((,) str . chain) production
        | all (isVariable . head . chain) rule = Just $ ((,) str . (chain . head)) rule -- fl4g{}
        | any (null . chain) rule              = Just (str, [])
        | otherwise                            = Nothing

match :: [Char] -> Production -> Bool
match token = maybe False (aux . fst) . (List.uncons . chain)
    where
    aux (Terminal auto) = Lexer.readable auto token
    aux _               = False

check :: Lexer.Token -> [Symbol]-> (Bool, [Symbol])
check _                 []                = (False, [])
check (Lexer.Token x t) prod1@(smbl:rest) = case smbl of
    Terminal auto -> Bool.bool (False, prod1) (True, rest) (Lexer.readable auto x)
    Variable rule -> case List.find (match x) rule of
        Just prod -> maybe (True, rest) ((,) True . (++ rest) . snd) (List.uncons $ chain prod)
        Nothing   -> if (not . any (null . chain)) rule
            then (False, prod1)
            else check (Lexer.Token x t) rest

alloc :: Lexer.Token -> Tree Lexer.Token -> Maybe (Tree Lexer.Token)
alloc (Lexer.Token x kind) tree = case tree of
    Node n l r -> case kind of
        Lexer.Operator_ -> case r of
            Empty -> Nothing
            _     -> if (precedence $ Lexer.Token x kind) > (precedence n) 
                then maybe Nothing (Just . Node n l) $ alloc (Lexer.Token x kind) r
                else Just $ Node (Lexer.Token x kind) tree Empty
        _         -> case l of
            Node _ _ _ -> maybe Nothing (Just . Node n l) $ alloc (Lexer.Token x kind) r
            Leaf _     -> maybe Nothing (Just . Node n l) $ alloc (Lexer.Token x kind) r
            Empty      -> Nothing
    Leaf n     -> case kind of
        Lexer.Operator_ -> Just $ Node (Lexer.Token x kind) (Leaf n) Empty
        _         -> Nothing
    Empty      -> case kind of
        Lexer.Operator_ -> Nothing
        _         -> Just $ Leaf (Lexer.Token x kind)

-- Grammar
mathExpr :: Tree Lexer.Token -> [Char]
mathExpr tree = case tree of
    Node (Lexer.Token x _) l r -> mathExpr l ++ x ++ mathExpr r
    Leaf (Lexer.Token x _)     -> x
    Empty                      -> ""
  
-- Grammar
setAxis :: Axis -> ExprMaker -> ExprMaker
setAxis k = ExprMaker k . expressions

-- data Parser = (Ast, Grammar) - { Grammar :: [Symbol] }
-- parser :: Lexer.Token -> Parser -> Either (Lexer.Token, Error) Parser

parser' :: Lexer.Token -> (ExprMaker, [Symbol]) -> Maybe (ExprMaker, [Symbol])
parser' (Lexer.Token x t) (pvec, stack) = if x == "" then Nothing 
    else case check (Lexer.Token x t) stack of
        (True, rest)  -> 
            let (a,b,c) = expressions pvec in 
                if elem t [Lexer.Operator_, Lexer.Integer_, Lexer.Double_, Lexer.Variable_] 
                    then case axis pvec of
                        X -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (n,b,c), rest)) 
                            $ alloc (Lexer.Token x t) a 
                        Y -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,n,c), rest)) 
                            $ alloc (Lexer.Token x t) b
                        Z -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,b,n), rest)) 
                            $ alloc (Lexer.Token x t) c
                    else Just (ExprMaker (if x == "," then succ $ axis pvec else axis pvec) (a,b,c), rest)
        (False, rest) -> Nothing

-- Grammar
magnitude :: Tree a -> Int
magnitude tree = case tree of
    Node _ l r -> 1 + magnitude l + magnitude r
    Leaf _     -> 1
    Empty      -> 0

-- type Expression = Tree Lexer.Token -- HERE!!

-- Grammar :: HERE !!!
-- Grammar
precedence :: Lexer.Token -> Int
precedence (Lexer.Token x _)
    | elem x ["*","/"] = 10
    | elem x ["+","-"] = 5
    | otherwise        = 0

-- parser :: [Char] -> Tree Lexer.Token -> Either (Tree Lexer.Token) (Char,Lexer.Automaton)
-- parser (x:xs) tree = 



module Struct.Compiler.Parser where

import Data.Either
import qualified Data.Maybe as Maybe 
import qualified Data.List as List
import qualified Data.Bool as Bool (bool)

import Struct.Math -- .Space
import qualified Struct.Compiler.Lexer as Lexer

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

-- First element of a (Production) must be a (Terminal _)
match :: [Char] -> Production -> Bool
match str = maybe False (aux . fst) . (List.uncons . chain)
    where
    aux (Terminal auto) = Lexer.readable auto str
    aux _               = False

-- (Token.valid,Stack.update)
check :: Lexer.Token -> [Symbol]-> (Bool, [Symbol])
check _                      []                = (False, [])
check (Lexer.Token str kind) prod1@(smbl:rest) = case smbl of
    Terminal auto -> Bool.bool (False, prod1) (True, rest) (Lexer.readable auto str)
    Variable rule -> case List.find (match str) rule of
        Just prod -> maybe (True, rest) ((,) True . (++ rest) . snd) (List.uncons $ chain prod)
        Nothing   -> if (not . any (null . chain)) rule
            then (False, prod1)
            else check (Lexer.Token str kind) rest

allocRight :: Lexer.Token -> Tree Lexer.Token -> Maybe (Tree Lexer.Token)
allocRight token (Node n l r) = maybe Nothing (Just . Node n l) (alloc token r)
allocRight _     _            = Nothing

allocLeft  :: Lexer.Token -> Tree Lexer.Token -> Maybe (Tree Lexer.Token)
allocLeft token (Node n l r) = maybe Nothing (\k -> Just $ Node n k r) (alloc token l)
allocLeft _     _            = Nothing

upLeft :: Tree Lexer.Token -> Tree Lexer.Token
upLeft (Node n l r) = l
upLeft k            = k    

rise :: Tree Lexer.Token -> Tree Lexer.Token
rise (Node n l r) = if Lexer.tokentype n == Lexer.None_ then l else (Node n (rise l) (rise r))
rise k            = k

closed :: Tree Lexer.Token -> Bool
closed Empty = False
closed (Leaf _) = True
closed (Node _ l r) = closed l && closed r

-- todo :: Add (sin,cos,tan :: Token _ Funct_) to (Tree Lexer.Token)

alloc :: Lexer.Token -> Tree Lexer.Token -> Maybe (Tree Lexer.Token)
alloc token (Node n l r) = case Lexer.tokentype token of
    Lexer.Operator_ ->  -- :: Token (..)
        case (Lexer.tokentype n) of
            Lexer.None_    -> Just (Node token l r)
            Lexer.Starter_ -> allocLeft token (Node n l r)
            _              -> case r of
                Empty -> Nothing
                _     -> if closed r
                    then if (precedence token) > (precedence n) 
                        then allocRight token (Node n l r)
                        else Just (Node token (Node n l r) Empty)
                    else allocRight token (Node n l r)
    Lexer.Starter_ ->  -- :: Token (..)
        case (Lexer.tokentype n) of
            Lexer.Starter_  -> allocLeft token (Node n l r)
            Lexer.None_     -> Nothing
            _               -> allocRight token (Node n l r)
    Lexer.Finisher_ -> -- :: Token (..)
        case (Lexer.tokentype n) of
            Lexer.Starter_  ->
                case allocLeft token (Node n l r) of
                    Nothing -> Just (Node (Lexer.Token "" Lexer.None_) (rise l) r)
                    Just k  -> Just k
            _               -> allocRight token (Node n l r)
    _         ->
        case l of
            Node m _ _ -> case Lexer.tokentype n of
                (Lexer.Starter_) -> allocLeft token (Node n l r)
                _                -> allocRight token (Node n l r)
            Leaf _     -> allocRight token (Node n l r)
            Empty      -> allocLeft token (Node n l r) -- Nothing

alloc token (Leaf n) = case Lexer.tokentype token of
    Lexer.Operator_ -> Just (Node token (Leaf n) Empty)
    _               -> Nothing
alloc token Empty = case Lexer.tokentype token of
    Lexer.Operator_ -> Nothing
    Lexer.Starter_  -> Just (Node token Empty Empty)
    Lexer.Finisher_ -> Nothing
    _               -> Just (Leaf token)

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

-- parser' :: Lexer.Token -> (ExprMaker, [Symbol]) -> Maybe (ExprMaker, [Symbol])
-- parser' (Lexer.Token x t) (pvec, stack) = if x == "" then Nothing 
--     else case check (Lexer.Token x t) stack of
--         (True, rest)  -> 
--             let (a,b,c) = expressions pvec in 
--                 if elem t [Lexer.Operator_, Lexer.Integer_, Lexer.Double_, Lexer.Variable_] 
--                     then case axis pvec of
--                         X -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (n,b,c), rest)) 
--                             $ alloc (Lexer.Token x t) a 
--                         Y -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,n,c), rest)) 
--                             $ alloc (Lexer.Token x t) b
--                         Z -> maybe Nothing (\n -> Just $ (ExprMaker (axis pvec) (a,b,n), rest)) 
--                             $ alloc (Lexer.Token x t) c
--                     else Just (ExprMaker (if x == "," then succ $ axis pvec else axis pvec) (a,b,c), rest)
--         (False, rest) -> Nothing

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
    -- | elem x ["(",")"] = 3
    | otherwise        = 0

-- parser :: [Char] -> Tree Lexer.Token -> Either (Tree Lexer.Token) (Char,Lexer.Automaton)
-- parser (x:xs) tree = 



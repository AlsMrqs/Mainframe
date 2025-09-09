module Parser where

import Data.Either
import Data.Maybe
import Data.List
import Graph.Automaton
import Graph.Grammar
import Lexer

-- Lexer --
open      = State None_ False [Transition ['('] $ State Starter_ True []]
close     = State None_ False [Transition [')'] $ State Finisher_ True []]
separator = State None_ False [Transition [','] $ State Separator_ True []]
-- End --

grammar = [ Production [ Terminal open
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal separator
                       , Variable expression
                       , Terminal close ] ]

expression = 
    [ Production [Terminal preNumber, Variable expansion]
    , Production [Terminal preIdentifier, Variable expansion] ]

expansion = [ Production [Terminal preOperator, Variable expression]
            , Production [] ]

-- alloc :: Char -> ([Char], Automaton) -> Maybe ([Char], Automaton)
-- alloc x (token, state) = case step x state of
--     Just s' -> Just (token ++ [x], s')
--     Nothing -> Nothing

type Error = [Char]

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty
    deriving (Eq, Show)

data PreVector = PreVector 
    { flag   :: Flag
    , forest :: (Tree Token, Tree Token, Tree Token) 
    } -- deriving Show

instance Show (PreVector) where
    show (PreVector fl (a,b,c)) =
        show a ++ "\n"
        ++ show b ++ "\n"
        ++ show c ++ "\n"
  
setFlag :: Flag -> PreVector -> PreVector
setFlag k prev = PreVector k $ forest prev

data Flag = X | Y | Z deriving (Show, Enum)

parser :: Token -> (PreVector, [Symbol]) -> Maybe (PreVector, [Symbol])
parser (Token x t) (pvec, stack) = if x == "" then Nothing else case check (Token x t) stack of
    (True, rest)  -> let (a,b,c) = forest pvec in 
        if elem t [Operator_, Integer_, Double_, Variable_] 
        then case flag pvec of
            X -> maybe Nothing (\n -> Just $ (PreVector (flag pvec) (n,b,c), rest)) 
                $ alloc (Token x t) a 
            Y -> maybe Nothing (\n -> Just $ (PreVector (flag pvec) (a,n,c), rest)) 
                $ alloc (Token x t) b
            Z -> maybe Nothing (\n -> Just $ (PreVector (flag pvec) (a,b,n), rest)) 
                $ alloc (Token x t) c
        else Just (PreVector (if x == "," then succ $ flag pvec else flag pvec) (a,b,c), rest)
    (False, rest) -> Nothing

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

magnitude :: Tree a -> Int
magnitude tree = case tree of
    Node _ l r -> 1 + magnitude l + magnitude r
    Leaf _     -> 1
    Empty      -> 0

calc :: Tree Token -> (Double,Double,Double) -> Double
calc tree (x,y,z) = case tree of
    Node n l r -> case (\(Token i _) -> i) n of
        "*" -> (calc l (x,y,z)) * (calc r (x,y,z))
        "/" -> (calc l (x,y,z)) / (calc r (x,y,z))
        "+" -> (calc l (x,y,z)) + (calc r (x,y,z))
        "-" -> (calc l (x,y,z)) - (calc r (x,y,z))
    Leaf (Token i t) -> case t of
        Variable_ -> case i of
            "x" -> x
            "X" -> x
            "y" -> y
            "Y" -> y
            "z" -> z
            "Z" -> z
        _         -> (Prelude.read i :: Double)
    Empty -> 0

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

precedence :: Token -> Int
precedence (Token x _)
    | elem x ["*","/"] = 10
    | elem x ["+","-"] = 5
    | otherwise        = 0

-- calculate :: Token -> CPU -> CPU

parser' :: [Char] -> Either Error [Char]
parser' []    = Left "Empty"
parser' input = 
    if isNothing production 
        then Left ("GLC can't read: " ++ token)
        else 
            if isRight . manager input $ chain (fromJust production) 
                then Right input
                else Left ("Parser.parser: manager - " ++ input)
    where
        (token, rest) = lexer input 
        production    = find (match token) grammar

    -- manager :: (...) -> Stack -> (...)
manager :: [Char] -> [Symbol] -> Either Error Bool
manager []    []    = Right True
manager []    stack = Left ("Input: 0 -|- Stack: " ++ (show $ length stack))
manager input []    = Left ("Input: " ++ (show $ length input) ++ "-|- Stack: 0")
manager input stack = 
    let (token, rest) = lexer input
        (symbol: _)   = stack
    in if token == "" 
        then Left ("Parser.manager: lexer - " ++ [head rest]) 
        else if isNothing (resolve token symbol)
            then Left ("Parser.manager: resolve - " ++ token)
            else 
                let (tok, prod) = fromJust (resolve token symbol) 
                in  manager (tok ++ rest) (prod ++ tail stack)


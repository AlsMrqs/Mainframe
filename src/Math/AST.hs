module Math.AST where

import qualified Graph as Graph
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Lexer   as Lexer
import qualified Math.Alphabet as Math.Alphabet

import qualified Control.Monad.State as State
import qualified Data.Map  as Map
import qualified Data.Bool as Bool 

data Expression 
    = Value Double    -- Element
    | Variable String -- Element

    | Subtraction    Expression Expression -- Operator
    | Addition       Expression Expression -- Operator
    | Multiplication Expression Expression -- Operator
    | Division       Expression Expression -- Operator

    -- later:
    -- | Open Expression Bool -- Enclose
    | Free 
    deriving (Show)

isEnclose :: Expression -> Bool
isEnclose exp = case exp of
    _ -> False

isEmpty :: Expression -> Bool
isEmpty exp = case exp of
    Subtraction    Free Free -> True
    Addition       Free Free -> True
    Multiplication Free Free -> True
    Division       Free Free -> True
    Free                     -> True
    _   -> False

isOperator :: Expression -> Bool
isOperator ast = case ast of
    (Variable _) -> False
    (Value _)    -> False
    Free         -> False
    _            -> True

isFull :: Expression -> Bool.Bool
isFull exp = case exp of
    (Subtraction a b) -> isFull a && isFull b
    (Addition a b)    -> isFull a && isFull b
    (Multiplication a b) -> isFull a && isFull b
    (Division a b)       -> isFull a && isFull b
    (Value _)         -> True
    (Variable _)      -> True
    Free              -> False

precedence :: Expression -> Int
precedence exp = case exp of
    (Multiplication _ _) -> 5
    (Division       _ _) -> 5
    (Subtraction _ _) -> 3
    (Addition    _ _) -> 3
    (Variable _) -> 1
    (Value _)    -> 1

higher :: Expression -> Expression -> (Expression,Expression)
higher a b = Bool.bool (b,a) (a,b) (precedence a >= precedence b)

toExpression :: Lexer.Token Math.Alphabet.Type -> Expression
toExpression x = case Lexer.label x of
    Math.Alphabet.Integer_  -> Value (read (Lexer.content x) :: Double)
    Math.Alphabet.Double_   -> Value (read (Lexer.content x) :: Double)
    Math.Alphabet.Operator_ -> toOperator x
    _                       -> Free

toOperator :: Lexer.Token Math.Alphabet.Type -> Expression
toOperator x = case Lexer.content x of
    "-" -> Subtraction Free Free
    "+" -> Addition    Free Free
    "*" -> Multiplication Free Free
    "/" -> Division       Free Free
    _   -> Free

insert :: Expression -> Expression -> Maybe Expression
insert obj ast  = case ast of
    -- (Multiplication _ _) -> 
    -- (Division       _ _) -> 
    (Subtraction _ _) -> insertIntoOperator obj ast
    (Addition    _ _) -> insertIntoOperator obj ast
    (Multiplication _ _) -> insertIntoOperator obj ast
    (Division _ _)       -> insertIntoOperator obj ast
    (Value _)         -> insertIntoElement obj ast
    -- (Variable _)         -> 
    Free             -> return obj
    _                -> return ast
    where

insertIntoElement :: Expression -> Expression -> Maybe Expression
insertIntoElement obj ast = case obj of
    (Subtraction _ _) -> insertIntoOperator ast obj
    (Addition    _ _) -> insertIntoOperator ast obj
    (Multiplication _ _) -> insertIntoOperator ast obj
    (Division _ _)       -> insertIntoOperator ast obj
    _                 -> return ast

    -- insertOperator :: Expression -> Expression -> Expression
    -- insertOperator x ast = case x of

insertIntoOperator :: Expression -> Expression -> Maybe Expression
insertIntoOperator obj ast = case ast of
    (Subtraction Free _) -> tryInsertLeft  obj ast
    (Subtraction _ Free) -> tryInsertRight obj ast
    (Subtraction a b)    -> if isFull ast 
        then tryCompose obj ast
        else insert obj b >>= return . Subtraction a
    (Addition Free _) -> tryInsertLeft  obj ast
    (Addition _ Free) -> tryInsertRight obj ast
    (Addition a b)    -> if isFull ast
        then tryCompose obj ast
        else insert obj b >>= return . Addition a
    (Multiplication Free _) -> tryInsertLeft  obj ast
    (Multiplication _ Free) -> tryInsertRight obj ast
    (Multiplication a b)    -> if isFull ast
        then tryCompose obj ast
        else insert obj b >>= return . Multiplication a
    (Division Free _) -> tryInsertLeft  obj ast
    (Division _ Free) -> tryInsertRight obj ast
    (Division a b)    -> if isFull ast
        then tryCompose obj ast
        else insert obj b >>= return . Division a
    _                    -> Nothing

tryInsertLeft :: Expression -> Expression -> Maybe Expression
tryInsertLeft obj ast = case ast of
    (Subtraction Free b) -> Bool.bool Nothing (return (Subtraction obj b)) 
        (isFull obj)
    (Addition Free b)    -> Bool.bool Nothing (return (Addition obj b)) 
        (isFull obj)
    (Multiplication Free b) -> Bool.bool Nothing (return (Multiplication obj b)) 
        (isFull obj)
    (Division Free b)    -> Bool.bool Nothing (return (Division obj b)) 
        (isFull obj)
    _                    -> Nothing

tryInsertRight :: Expression -> Expression -> Maybe Expression
tryInsertRight obj ast = case ast of
    (Subtraction a Free) -> Bool.bool Nothing (return (Subtraction a obj)) 
        (isFull obj || isEnclose obj)
    (Addition a Free) -> Bool.bool Nothing (return (Addition a obj)) 
        (isFull obj || isEnclose obj)
    (Multiplication a Free) -> Bool.bool Nothing (return (Multiplication a obj)) 
        (isFull obj || isEnclose obj)
    (Division a Free) -> Bool.bool Nothing (return (Division a obj)) 
        (isFull obj || isEnclose obj)
    _ -> Nothing

tryCompose :: Expression -> Expression -> Maybe Expression
tryCompose obj ast = case ast of
    (Subtraction a b) -> if not (isOperator obj && isEmpty obj) 
        then Nothing
        else
            if (precedence obj <= precedence ast)
                then insertIntoOperator ast obj
                else insert obj b >>= return . Subtraction a
    (Addition a b) -> if not (isOperator obj && isEmpty obj) 
        then Nothing
        else
            if (precedence obj <= precedence ast)
                then insertIntoOperator ast obj
                else insert obj b >>= return . Addition a
    (Multiplication a b) -> if not (isOperator obj && isEmpty obj) 
        then Nothing
        else
            if (precedence obj <= precedence ast)
                then insertIntoOperator ast obj
                else insert obj b >>= return . Multiplication a
    (Division a b) -> if not (isOperator obj && isEmpty obj) 
        then Nothing
        else
            if (precedence obj <= precedence ast)
                then insertIntoOperator ast obj
                else insert obj b >>= return . Division a
    _ -> Nothing


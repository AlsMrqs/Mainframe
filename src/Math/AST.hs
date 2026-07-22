module Math.AST where

import qualified Graph as Graph
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Lexer   as Lexer
import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron   as Math.Neuron

import qualified Control.Monad.State as State
-- import qualified Control.Alternativa as Alternative
import qualified Data.Map  as Map
import qualified Data.Bool as Bool 

data Expression 
    = Power          Expression Expression -- Operator
    | Multiplication Expression Expression -- Operator
    | Division       Expression Expression -- Operator
    | Subtraction    Expression Expression -- Operator
    | Addition       Expression Expression -- Operator
    | Enclosed String Expression String Bool -- Structure
    | Value Double    -- Element
    | Variable String -- Element
    | Free 

-- todo:
-- | Negate Expression -- Negate : (Value _) | (Variable _) | (Enclosed ...)

instance Show Expression where
    show (Power a b)          = "((^) " ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Multiplication a b) = "((*) " ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Division a b)       = "((/) " ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Subtraction a b)    = "((-) " ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Addition a b)       = "((+) " ++ (show a) ++ " " ++ (show b) ++ ")"
    show (Enclosed l x r _)   = (show l) ++ " " ++ (show x) ++ " " ++ (show r)
    show (Value x)            = (show x)
    show (Variable x)         = x
    -- show (Negate x)           = "-" ++ (show x)
    show Free                 = "_"

arity :: Expression -> Int
arity exp = case exp of
    (Power _ _)          -> 2
    (Multiplication _ _) -> 2
    (Division _ _)       -> 2
    (Subtraction _ _)    -> 2
    (Addition _ _)       -> 2
    (Value _)            -> 0
    (Variable _)         -> 0
    (Enclosed _ _ _ _)   -> 1
    -- (Negate _)           -> 1
    Free                 -> 1

free :: Expression -> Int
free exp = case exp of
    (Power          l r) -> (+) (free l) (free r)
    (Multiplication l r) -> (+) (free l) (free r)
    (Division       l r) -> (+) (free l) (free r)
    (Subtraction    l r) -> (+) (free l) (free r)
    (Addition       l r) -> (+) (free l) (free r)
    (Value    _)         -> 0
    (Variable _)         -> 0
    (Enclosed _ k _ off) -> Bool.bool 1 0 off
    -- (Negate x)           -> free x
    Free                 -> 1

isFull :: Expression -> Bool.Bool
isFull exp = case exp of
    (Power          a b) -> (&&) (free a == 0) (free b == 0)
    (Multiplication a b) -> (&&) (free a == 0) (free b == 0)
    (Division       a b) -> (&&) (free a == 0) (free b == 0)
    (Subtraction    a b) -> (&&) (free a == 0) (free b == 0)
    (Addition       a b) -> (&&) (free a == 0) (free b == 0)
    (Enclosed _ k _ off) -> off -- Bool.bool False True off
    -- (Negate x)           -> free x == 0
    (Value    _)         -> True
    (Variable _)         -> True
    Free                 -> False

isEnclose :: Expression -> Bool
isEnclose (Enclosed _ _ _ _) = True
isEnclose _                  = False

-- todo: 

turnOffEnclosed :: Math.Alphabet.Type -> Expression -> Maybe Expression
turnOffEnclosed typ exp = case exp of
    (Power          a b) -> (turnOffEnclosed typ b) >>= return . Power a
    (Multiplication a b) -> (turnOffEnclosed typ b) >>= return . Multiplication a 
    (Division       a b) -> (turnOffEnclosed typ b) >>= return . Division a
    (Subtraction    a b) -> (turnOffEnclosed typ b) >>= return . Subtraction a
    (Addition       a b) -> (turnOffEnclosed typ b) >>= return . Addition a
    (Enclosed l k r off) -> 
        if off 
            then Nothing 
            else return $ maybe 
                (Enclosed l k r True) -- (compareType r typ)) 
                (\x -> Enclosed l x r off) 
                (turnOffEnclosed typ k)
    -- (Negate x)           -> turnOffEnclosed typ x
    (Value    _)         -> Nothing
    (Variable _)         -> Nothing
    Free                 -> Nothing

compareType :: [Char] -> Math.Alphabet.Type -> Bool
compareType str typ = ((Lexer.label $ fmap Lexer.unkind token) == typ)
    where
    (token,remainder) = Math.Neuron.lex str

------------------------------------------------------------------
isOperator :: Expression -> Bool
isOperator ast = case ast of
    (Enclosed _ _ _ _) -> False
    (Variable _)       -> False
    (Value    _)       -> False
    Free               -> False
    -- (Negate _)         -> False
    _                  -> True

precedence :: Expression -> Int
precedence exp = case exp of
    (Power          _ _) -> 7
    (Multiplication _ _) -> 5
    (Division       _ _) -> 5
    (Subtraction _ _) -> 3
    (Addition    _ _) -> 3
    (Variable _)       -> 1
    (Value    _)       -> 1
    (Enclosed _ _ _ _) -> 1
    -- (Negate _)         -> 1

toExpression :: Lexer.Token Math.Alphabet.Type -> Expression
toExpression x = case Lexer.label x of
    Math.Alphabet.Integer_  -> Value (read (Lexer.content x) :: Double)
    Math.Alphabet.Double_   -> Value (read (Lexer.content x) :: Double)
    Math.Alphabet.Variable_ -> Variable (Lexer.content x)
    Math.Alphabet.Operator_ -> toOperator x
    Math.Alphabet.ParenthesisOpen_ -> Enclosed "(" Free ")" False
    Math.Alphabet.BracketOpen_     -> Enclosed "[" Free "]" False 
    Math.Alphabet.BracesOpen_      -> Enclosed "{" Free "}" False
    _                       -> Free
    where
    toOperator :: Lexer.Token Math.Alphabet.Type -> Expression
    toOperator x = case Lexer.content x of
        "^" -> Power          Free Free
        "*" -> Multiplication Free Free
        "/" -> Division       Free Free
        "-" -> Subtraction    Free Free
        "+" -> Addition       Free Free
        _   -> Free

--------------------------------------------------------------------------------

insert :: Expression -> Expression -> Maybe Expression
insert obj ast  = case ast of
    (Power          _ _) -> obj `insertIntoOperator` ast
    (Multiplication _ _) -> obj `insertIntoOperator` ast
    (Division _ _)       -> obj `insertIntoOperator` ast
    (Subtraction _ _)    -> obj `insertIntoOperator` ast
    (Addition    _ _)    -> obj `insertIntoOperator` ast
    (Enclosed _ _ _ _)   -> obj `insertIntoEnclosed` ast
    (Value _)            -> obj `embodyElement` ast
    (Variable _)         -> obj `embodyElement` ast
    Free                 -> return obj

insertIntoEnclosed :: Expression -> Expression -> Maybe Expression
insertIntoEnclosed obj (Enclosed l exp r True)  = if isOperator obj 
    then insert (Enclosed l exp r True) obj 
    else Nothing
insertIntoEnclosed obj (Enclosed l exp r False) = insert obj exp 
    >>= \k -> return (Enclosed l k r False)

embodyElement :: Expression -> Expression -> Maybe Expression
embodyElement obj ele = case obj of
    (Power _ _)          -> insertIntoOperator ele obj
    (Multiplication _ _) -> insertIntoOperator ele obj
    (Division _ _)       -> insertIntoOperator ele obj
    (Subtraction _ _)    -> insertIntoOperator ele obj
    (Addition    _ _)    -> insertIntoOperator ele obj
    (Enclosed _ _ _ _)   -> Nothing
    (Value _)            -> Nothing
    (Variable _)         -> Nothing
    Free                 -> return ele

insertIntoOperator :: Expression -> Expression -> Maybe Expression
insertIntoOperator obj oper = case oper of
    (Power Free _) -> tryInsertLeft  obj oper
    (Power _ Free) -> tryInsertRight obj oper
    (Power a b)    -> if isFull (Power a b)
        then tryCompose obj (Power a b)
        else insert obj b >>= return . Power a

    (Multiplication Free _) -> tryInsertLeft  obj oper
    (Multiplication _ Free) -> tryInsertRight obj oper
    (Multiplication a b)    -> if isFull (Multiplication a b)
        then tryCompose obj (Multiplication a b)
        else insert obj b >>= return . Multiplication a

    (Division Free _) -> tryInsertLeft  obj oper
    (Division _ Free) -> tryInsertRight obj oper
    (Division a b)    -> if isFull (Division a b)
        then tryCompose obj (Division a b) 
        else insert obj b >>= return . Division a

    (Subtraction Free _) -> tryInsertLeft  obj oper
    (Subtraction _ Free) -> tryInsertRight obj oper
    (Subtraction a b)    -> if isFull (Subtraction a b)
        then tryCompose obj (Subtraction a b)
        else insert obj b >>= return . Subtraction a

    (Addition Free _) -> tryInsertLeft  obj oper
    (Addition _ Free) -> tryInsertRight obj oper
    (Addition a b)    -> if isFull (Addition a b)
        then tryCompose obj (Addition a b)
        else insert obj b >>= return . Addition a

    _                    -> Nothing

----------------------- Good ------------------
tryInsertLeft :: Expression -> Expression -> Maybe Expression
tryInsertLeft obj ast = case ast of
    (Power Free b) -> Bool.bool Nothing (return (Power obj b))
        (isFull obj)
    (Multiplication Free b) -> Bool.bool Nothing (return (Multiplication obj b)) 
        (isFull obj)
    (Division Free b)    -> Bool.bool Nothing (return (Division obj b)) 
        (isFull obj)
    (Subtraction Free b) -> Bool.bool Nothing (return (Subtraction obj b)) 
        (isFull obj)
    (Addition Free b)    -> Bool.bool Nothing (return (Addition obj b)) 
        (isFull obj)
    _                    -> Nothing

tryInsertRight :: Expression -> Expression -> Maybe Expression
tryInsertRight obj ast = case ast of
    (Power a Free) -> Bool.bool Nothing (return (Power a obj))
        (isFull obj || isEnclose obj)
    (Multiplication a Free) -> Bool.bool Nothing (return (Multiplication a obj)) 
        (isFull obj || isEnclose obj)
    (Division a Free) -> Bool.bool Nothing (return (Division a obj)) 
        (isFull obj || isEnclose obj)
    (Subtraction a Free) -> Bool.bool Nothing (return (Subtraction a obj)) 
        (isFull obj || isEnclose obj)
    (Addition a Free) -> Bool.bool Nothing (return (Addition a obj)) 
        (isFull obj || isEnclose obj)
    _ -> Nothing
------------------------------------------------

tryCompose :: Expression -> Expression -> Maybe Expression
tryCompose obj oper = case oper of
    (Power a b) -> if not (isOperator obj && (not . isFull) obj) 
        then Nothing
        else
            if (precedence obj <= precedence oper)
                then insertIntoOperator oper obj
                else insert obj b >>= return . Power a
    (Multiplication a b) -> if not (isOperator obj && (not . isFull) obj) 
        then Nothing
        else
            if (precedence obj <= precedence oper)
                then insertIntoOperator oper obj
                else insert obj b >>= return . Multiplication a
    (Division a b) -> if not (isOperator obj && (not . isFull) obj) 
        then Nothing
        else
            if (precedence obj <= precedence oper)
                then insertIntoOperator oper obj
                else insert obj b >>= return . Division a
    (Subtraction a b) -> if not (isOperator obj && (not . isFull) obj) 
        then Nothing
        else
            if (precedence obj <= precedence oper)
                then insertIntoOperator oper obj
                else insert obj b >>= return . Subtraction a
    (Addition a b) -> if not (isOperator obj && (not . isFull) obj) 
        then Nothing
        else
            if (precedence obj <= precedence oper)
                then insertIntoOperator oper obj
                else insert obj b >>= return . Addition a
    _ -> Nothing


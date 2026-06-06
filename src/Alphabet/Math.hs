module Math.Alphabet where

import qualified Graph as Graph
import qualified Folklore.Grammar.Lexer as Lexer

type Alphabet = Graph.Graph (Lexer.Token Type) Char

data Type = Starter_ | Separator_ | Finisher_ 
    | Punctuation_ 
    | Operator_
    | Variable_ 
    | Integer_ | Double_ | None_ 
    deriving (Show, Eq)

starter     :: [Char]
separator   :: [Char]
finisher    :: [Char]
punctuation :: [Char]
operator    :: [Char]
negative    :: [Char]
positive    :: [Char]
variable    :: [Char]
integer     :: [Char]
notation    :: [Char]

starter     = ['(','[','{']
separator   = [',']
finisher    = [')',']','}']
punctuation = ['.']
operator    = ['+','-','*','/','^','#','!']
variable    = '_' :  (['a'..'z'] ++ ['A'..'Z'])
integer     = ['0'..'9']
notation    = ['e','E']

token :: Char -> Type
token k
    | elem k starter     = Starter_
    | elem k separator   = Separator_
    | elem k finisher    = Finisher_
    | elem k punctuation = Punctuation_
    | elem k operator    = Operator_
    | elem k variable    = Variable_
    | elem k interger    = Integer_
    | otherwise          = None_


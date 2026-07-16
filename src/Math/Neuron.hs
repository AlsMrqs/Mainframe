module Math.Neuron where

import qualified Graph as Graph
import qualified Folklore.Lexer as Lexer
import qualified Math.Alphabet as Math.Alphabet

lex :: [Char] -> (Lexer.Token (Lexer.Kind Math.Alphabet.Type), [Char])
lex = Lexer.lex Math.Neuron.start . dropWhile ((==) ' ')

type Neuron = Graph.Graph (Lexer.Kind Math.Alphabet.Type) Char

start :: Neuron
start = Graph.Vertex (Lexer.Kind Math.Alphabet.None_ Lexer.Reject) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Starter_   -> return starter
        Math.Alphabet.Separator_ -> return separator
        Math.Alphabet.Finisher_  -> return finisher
        Math.Alphabet.Operator_  -> return operator
        Math.Alphabet.Integer_   -> return integer
        Math.Alphabet.Variable_  -> return variable
        _ -> Nothing

starter :: Neuron
starter = Graph.Vertex (Lexer.Kind Math.Alphabet.Starter_ Lexer.Accept) (const Nothing)

separator :: Neuron
separator = Graph.Vertex (Lexer.Kind Math.Alphabet.Separator_ Lexer.Accept) (const Nothing)

finisher :: Neuron
finisher = Graph.Vertex (Lexer.Kind Math.Alphabet.Finisher_ Lexer.Accept) (const Nothing)

operator :: Neuron
operator = Graph.Vertex (Lexer.Kind Math.Alphabet.Operator_ Lexer.Accept) (const Nothing)

integer :: Neuron
integer = Graph.Vertex (Lexer.Kind Math.Alphabet.Integer_ Lexer.Accept) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Integer_     -> return integer
        Math.Alphabet.Punctuation_ -> return point
        _ -> Nothing

point :: Neuron
point = Graph.Vertex (Lexer.Kind Math.Alphabet.Punctuation_ Lexer.Reject) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Integer_ -> return double
        _ -> Nothing

double :: Neuron
double = Graph.Vertex (Lexer.Kind Math.Alphabet.Double_ Lexer.Accept) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Integer_ -> return double
        _ -> Nothing

variable :: Neuron
variable = Graph.Vertex (Lexer.Kind Math.Alphabet.Variable_ Lexer.Accept) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Variable_ -> return variable 
        Math.Alphabet.Integer_ -> return variable
        _ -> Nothing


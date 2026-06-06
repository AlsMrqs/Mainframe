module Math.Neuron where

import qualified Graph as Graph
import qualified Folklore.Lexer as Lexer
import qualified Math.Alphabet as Math

type Neuron = Graph.Graph (Lexer.Kind Math.Type) Char

start :: Neuron
start = Graph.Vertex (Lexer.Kind Math.None_ Lexer.Reject) context
    where
    context = \k -> case Math.token k of
        Math.Starter_   -> return starter
        Math.Separator_ -> return separator
        Math.Finisher_  -> return finisher
        Math.Operator_  -> return operator
        Math.Integer_   -> return integer
        _ -> Nothing

starter :: Neuron
starter = Graph.Vertex (Lexer.Kind Math.Starter_ Lexer.Accept) (const Nothing)

separator :: Neuron
separator = Graph.Vertex (Lexer.Kind Math.Separator_ Lexer.Accept) (const Nothing)

finisher :: Neuron
finisher = Graph.Vertex (Lexer.Kind Math.Finisher_ Lexer.Accept) (const Nothing)

operator :: Neuron
operator = Graph.Vertex (Lexer.Kind Math.Operator_ Lexer.Accept) (const Nothing)

integer :: Neuron
integer = Graph.Vertex (Lexer.Kind Math.Integer_ Lexer.Accept) context
    where
    context = \k -> case Math.token k of
        Math.Integer_     -> return integer
        Math.Punctuation_ -> return point
        _ -> Nothing

point :: Neuron
point = Graph.Vertex (Lexer.Kind Math.Punctuation_ Lexer.Reject) context
    where
    context = \k -> case Math.token k of
        Math.Integer_ -> return double
        _ -> Nothing

double :: Neuron
double = Graph.Vertex (Lexer.Kind Math.Double_ Lexer.Accept) context
    where
    context = \k -> case Math.token k of
        Math.Integer_ -> return double
        _ -> Nothing


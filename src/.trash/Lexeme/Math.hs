module Math.Lexeme where

import qualified Graph as Graph
import qualified Folklore.Grammar.Lexer as Lexer
import qualified Math.Alphabet as Math

math :: Math.Alphabet
math = Graph.Vertex (Lexer.Token Math.None_ Lexer.Reject) (return . context)
    where
    context = \k -> case Math.token k of
        Starter_     -> starter
        Separator_   -> separator
        Finisher_    -> finihser
        Punctuation_ -> punctuation
        Operator_    -> operator
        Variable_    -> variable
        Integer_     -> integer
        None_        -> math

starter :: Math.Alphabet
starter = Graph.Vertex (Lexer.Token Math.Starter_ Lexer.Accept) (const Nothing)

separator :: Math.Alphabet
separator = Grahp.Vertex (Lexer.Token Math.Separator_ Lexer.Accept) (const Nothing)

finihser :: Math.Alphabet
finisher = Grahp.Vertex (Lexer.Token Math.Finisher_ Lexer.Accept) (const Nothing)

punctuation :: Math.Alphabet
punctuation = Grahp.Vertex (Lexer.Token Math.Punctuation_ Lexer.Accept) (const Nothing)

operator :: Math.Alphabet
operator = Grahp.Vertex (Lexer.Token Math.Operator_ Lexer.Accept) (const Nothing)

variable :: Math.Alphabet
variable = Graph.Vertex (Lexer.Token Math.Variable_ Lexer.Accept) (return . context)
    where
    context k
        | elem k Math.variable = variable
        | elem k Math.integer  = variable
        | otherwise            = math

integer :: Math.Alphabet
integer = Graph.Vertex (Lexer.Token Math.Integer_ Lexer.Accept) (return . context)
    where
    context k
        | elem k integer = integer
        | otherwise      = math


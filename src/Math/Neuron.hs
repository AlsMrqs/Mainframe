module Math.Neuron where

import qualified Graph as Graph
import qualified Folklore.Lexer as Lexer
import qualified Math.Alphabet as Math.Alphabet
import qualified Data.Bool as Bool

lex :: [Char] -> (Lexer.Token (Lexer.Kind Math.Alphabet.Type), [Char])
lex = Lexer.lex Math.Neuron.start . dropWhile ((==) ' ')

lexer :: [Char] -> Either [Char] [Lexer.Token Math.Alphabet.Type]
lexer []  = return []
lexer str = let payload = dropWhile ((==) ' ') str
    in Bool.bool catch (lexer payload) (null payload)
    where
    (tokenKind, remainder) = Math.Neuron.lex str
    catch = Bool.bool store block (Lexer.flag (Lexer.label tokenKind) == Lexer.Reject)
    block = Left ("Invalid input! " ++ show (tokenKind))
    store = lexer remainder >>= return . (:) (fmap Lexer.unkind tokenKind)

type Neuron = Graph.Graph (Lexer.Kind Math.Alphabet.Type) Char

start :: Neuron
start = Graph.Vertex (Lexer.Kind Math.Alphabet.None_ Lexer.Reject) context
    where
    context = \k -> case Math.Alphabet.token k of
        Math.Alphabet.Starter_   -> return starter
        Math.Alphabet.Separator_ -> return separator
        Math.Alphabet.Finisher_  -> return finisher

        Math.Alphabet.ParenthesisOpen_ -> return parenthesisOpen
        Math.Alphabet.ParenthesisClose_ -> return parenthesisClose
        Math.Alphabet.BracketOpen_ -> return bracketOpen
        Math.Alphabet.BracketClose_ -> return bracketClose
        Math.Alphabet.BracesOpen_ -> return bracesOpen
        Math.Alphabet.BracesClose_ -> return bracesClose

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

parenthesisOpen :: Neuron
parenthesisOpen = Graph.Vertex (Lexer.Kind Math.Alphabet.ParenthesisOpen_ Lexer.Accept) 
    (const Nothing)

parenthesisClose :: Neuron
parenthesisClose = Graph.Vertex (Lexer.Kind Math.Alphabet.ParenthesisClose_ Lexer.Accept) 
    (const Nothing)

bracketOpen :: Neuron
bracketOpen = Graph.Vertex (Lexer.Kind Math.Alphabet.BracketOpen_ Lexer.Accept) 
    (const Nothing)

bracketClose :: Neuron
bracketClose = Graph.Vertex (Lexer.Kind Math.Alphabet.BracketClose_ Lexer.Accept) 
    (const Nothing)

bracesOpen :: Neuron
bracesOpen = Graph.Vertex (Lexer.Kind Math.Alphabet.BracesOpen_ Lexer.Accept) 
    (const Nothing)

bracesClose :: Neuron
bracesClose = Graph.Vertex (Lexer.Kind Math.Alphabet.BracesClose_ Lexer.Accept) 
    (const Nothing)

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


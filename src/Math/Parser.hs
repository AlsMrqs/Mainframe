module Math.Parser where

import qualified Control.Monad.State as State
import qualified Data.Either as Either
import qualified Data.Bool   as Bool

import qualified Folklore.Lexer   as Lexer
import qualified Folklore.Grammar as Grammar
import qualified Folklore.Parser  as Parser

import qualified Math.Alphabet as Math.Alphabet
import qualified Math.Neuron   as Math.Neuron
import qualified Math.AST      as Math.AST

type AST        = Math.AST.Expression
type Math       = Math.Alphabet.Type
type Pushdown a = Parser.Pushdown a
type Transition a = Pushdown a -> State.StateT AST (Either.Either [Char]) (Pushdown Math)

parse :: String -> Pushdown Math -> AST -> Either [Char] AST
parse []  _        ast = return ast
parse str pushdown ast = let (tokenKind,remainder) = Math.Neuron.lex str
    in
    case (Lexer.flag . Lexer.label) tokenKind of
        Lexer.Reject -> Left ("Invid input!" ++ (show tokenKind))
        Lexer.Accept -> 
            State.runStateT (allocate (fmap Lexer.unkind tokenKind)) ast 
                >>= \(a,b) -> parse (dropWhile ((==) ' ') remainder) a b
    where

    allocate :: Lexer.Token Math -> State.StateT AST (Either.Either [Char]) (Pushdown Math)
    allocate tok = control pushdown
        where

        control :: Pushdown Math -> State.StateT AST (Either.Either [Char]) (Pushdown Math)
        control pushdown' = do
            ast' <- State.get
            let transition = Grammar.transition (Lexer.label tok)

            case State.runStateT transition (Parser.grammar pushdown') of
                -- [+] - verify stack
                Nothing -> do
                    case (Parser.peak pushdown') of
                        Nothing -> State.lift $ 
                            Left ("Invalid gramatical input (in): " ++ show tok)
                        Just k  -> 
                            if Lexer.label tok == k
                                then return pushdown' -- remove from stack -> add into AST
                                else State.lift $ 
                                    Left ("Invalid gramatical input (in): " ++ show tok)
                    
                Just (stk,gram')  -> do
                    State.put . maybe ast' id $
                        (Math.AST.insert (Math.AST.toExpression tok) ast')
                    return ((Parser.push stk . Parser.pointer gram') pushdown')


module Compiler.Language.Grammar where

import Data.Maybe
-- import Data.Tree
import Data.List
import Prelude hiding (lex)

import Compiler.Language.Dictionary
import Compiler.Lexer
import Compiler.Parser

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

token :: ([Char],Automaton) -> Token
token (str,auto) = Token str (typeof auto)

lexer :: [Char] -> ([Char],Automaton) -> (Token,[Char])
lexer []     = flip (,) [] . token
lexer (x:xs) = either (flip (,) (x:xs) . fst) (lexer xs) . lexer' x

lex = flip lexer ([],machine)

-- todo!!!!!!
parse' :: Token -> (Tree Token,[Symbol]) -> Either String (Tree Token,[Symbol])
parse' token (tree,stack)  = let (accepted,newStack) = check token stack in
    if not accepted
        then Left $ (++) "Grammar can't recognize: " (show token)
        else case alloc token tree of
            Nothing      -> Left  $ (++) "AST can't alloc: " (show token)
            Just newTree -> Right (newTree,newStack)


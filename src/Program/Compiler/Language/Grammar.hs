module Compiler.Language.Grammar where

import Data.Maybe
import Data.Tree
import Data.List

import Compiler.Language.Dictionary
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


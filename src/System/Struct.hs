module System.Struct where

import Data.Maybe as Maybe
import Data.List as List

import System.Program

data System = Node
    { program :: Program
    , caller  :: Maybe Node
    , link    :: [Node] }

access :: String -> Node ->  Maybe Node
access str node = List.find ((==) str . name . program) . link $ node

data Graph a k = Vertex 
    { vert :: a 
    , edge :: [k -> Maybe (Graph a k)] }

instance (Show a) => Show (Graph a k) where
    show = (++) "Vertex " . show . vert

cross :: k -> Graph a k -> Maybe (Graph a k)
cross x = Maybe.maybe Nothing (flip ($) x) 
    . List.find (Maybe.isJust . flip ($) x) 
    . edge


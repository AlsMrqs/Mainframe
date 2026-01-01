module Kernel.Struct where

import Data.Maybe as Maybe
import Data.List as List

-- import Data.Map as Map
-- import Engine.Math.Space as Math

data Graph a k = Vertex 
    { vert :: a 
    , edge :: [k -> Maybe (Graph a k)] }

instance (Show a) => Show (Graph a k) where
    show = (++) "Vertex " . show . vert

cross :: k -> Graph a k -> Maybe (Graph a k)
cross x = Maybe.maybe Nothing (flip ($) x) 
    . List.find (Maybe.isJust . flip ($) x) 
    . edge

-- Graph Figure Position -- Mouse Position change Figure State

-- cross :: Eq k => k -> Graph a k -> Maybe (Graph a k)
-- cross x = maybe Nothing (Just . snd) . List.find (any ((==) x) . fst) . edge

-- cross :: k ->  Graph a (k -> Bool) -> Maybe (Graph a (k -> Bool))
-- cross x = maybe Nothing (Just . snd) . List.find (flip ($) x . fst) . edge
-- 
-- s0 = Vertex "Something" [(\x -> elem x "abc", s1)]
-- s1 = Vertex "Nothing" []


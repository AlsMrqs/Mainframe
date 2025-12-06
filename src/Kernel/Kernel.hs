module Kernel where

import Data.List as List

data Graph a k = Vertex 
    { vert :: a 
    , edge :: [([k], Graph a k)] }

instance (Show a) => Show (Graph a k) where
    show = (++) "Vertex " . show . vert

cross :: Eq k => k -> Graph a k -> Maybe (Graph a k)
cross x = maybe Nothing (Just . snd) . List.find (any ((==) x) . fst) . edge


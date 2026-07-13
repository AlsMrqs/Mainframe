module Graph where

import qualified Control.Monad.State as State
import qualified Data.Map as Map

data Graph a k = Vertex
    { vert :: a 
    , edge :: k -> Maybe (Graph a k) }

instance (Show a) => Show (Graph a k) where
    show = (++) "Vertex " . show . vert

instance (Eq a) => Eq (Graph a k) where
    (==) x y = (vert x) == (vert y)

instance (Ord a) => Ord (Graph a k) where
   (<=) x y = (vert x) <= (vert y)
   (>=) x y = (vert x) >= (vert y)

cross :: k -> Graph a k -> Maybe (Graph a k)
cross x = flip ($) x . edge

search :: Ord a => k -> Map.Map a b -> State.State (Graph a k) (Maybe b)
search x _lib = do
    s0 <- State.get
    case cross x s0 of
        Nothing -> return Nothing
        Just s1 -> State.put s1 >> return (Map.lookup (vert s1) _lib)

-- Content of cross in _lib


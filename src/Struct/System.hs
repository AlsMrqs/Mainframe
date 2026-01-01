module Struct.System where
import Control.Monad.State 
import Data.Map as Map
import Struct.Space as Space

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

instance (Space.Coordinate a) => Space.Coordinate (Graph a k) where
    coord axis = Space.coord axis . vert

cross :: k -> Graph a k -> Maybe (Graph a k)
cross x = flip ($) x . edge

search :: Ord a => k -> Map a b -> State (Graph a k) (Maybe b)
search x _lib = do
    s0 <- get
    case cross x s0 of
        Nothing -> return Nothing
        Just s1 -> put s1 >> return (Map.lookup (vert s1) _lib)


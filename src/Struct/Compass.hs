module Struct.Compass where
import Struct.System as System
import Struct.Space  as Space
import Data.Bool     as Bool

type Compass a = System.Graph a Energy
data Energy    = N | NE | E | SE | S | SW | W | NW deriving (Eq,Show)
instance Semigroup Energy where
    (<>) N E = NE
    (<>) E N = NE
    (<>) E S = SE
    (<>) S E = SE
    (<>) S W = SW
    (<>) W S = SW 
    (<>) W N = NW
    (<>) N W = NW

-- todo : Maybe
-- class (Space.Coordinate a) => Singularity (System.Graph a k) where

create :: Space.Coordinate a => a -> Compass a
create p = System.Vertex p (\_ -> Nothing)

direction :: Space.Coordinate a => a -> Compass a -> Maybe Energy
direction k o = (fmap (<>) dx) <*> dy
    where
    (x',y',_) = Space.point k
    (x, y, _) = Space.point (vert o)
    dx        = difference X x' x
    dy        = difference Y y' y

    difference :: (Ord a) => Axis -> a -> a -> Maybe Energy
    difference axis k o = if k == o then Nothing 
        else case axis of
            X -> return $ bool W E (o < k)
            Y -> return $ bool S N (o < k)
            _ -> Nothing

insert :: (Space.Coordinate a) => a -> Compass a -> Compass a
insert p o = case direction p o of
    Nothing -> o { vert = p }
    Just k  -> let
        f N  = case (k == N)  of True  -> (edge o) N  >>= return . insert p
                                 False -> (edge o) N
        f NE = case (k == NE) of True  -> (edge o) NE >>= return . insert p
                                 False -> (edge o) NE
        f E  = case (k == E)  of True  -> (edge o) E  >>= return . insert p
                                 False -> (edge o) E
        f SE = case (k == SE) of True  -> (edge o) SE >>= return . insert p
                                 False -> (edge o) SE
        f S  = case (k == S)  of True  -> (edge o) S  >>= return . insert p
                                 False -> (edge o) S
        f SW = case (k == SW) of True  -> (edge o) SW >>= return . insert p
                                 False -> (edge o) SW
        f W  = case (k == W)  of True  -> (edge o) W  >>= return . insert p
                                 False -> (edge o) W
        f NW = case (k == NW) of True  -> (edge o) NW >>= return . insert p
                                 False -> (edge o) NW
        in o { edge = f }


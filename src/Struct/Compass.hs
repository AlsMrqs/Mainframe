module Struct.Compass where
import qualified Data.Bool as Bool
import Struct.System as System
import Struct.Screen as Screen
import Struct.Space  as Space
import Struct.Mouse  as Mouse

import Struct.Math   as Math
targets = Math.isInsidePolygon

type Overline a = System.Graph (Center a) Energy
data Energy     = N | NE | E | SE | S | SW | W | NW 
    deriving (Eq,Show)

data Center a = Center
    { origin ::  (Space.Coord, Space.Coord, Space.Coord)
    , colony :: [a] }

instance Eq (Center k) where
    (==) a b = az == bz
        where
        (_,_,az) = origin a
        (_,_,bz) = origin b

instance Ord (Center k) where -- (for) DBMS
    (<=) a b = az <= bz
        where
        (_,_,az) = origin a
        (_,_,bz) = origin b
    (>=) a b = az >= bz
        where
        (_,_,az) = origin a
        (_,_,bz) = origin b

instance (Space.Coordinate a) => Space.Coordinate (Center a) where
    coord axis k = let (x,y,z) = origin k
        in case axis of
            X -> x
            Y -> y
            Z -> z

append :: a -> Center a -> Center a
append x c = c { colony = x : (colony c) }

constellation = Screen.ordin -- constellation : ambiguos
center        = Space.point . System.vert

type Spectrum = Screen.Interactive Mouse.Track
    
direction :: Spectrum -> Overline Spectrum -> Maybe Energy
direction spectr compass =
    case (center compass) `targets` (constellation $ System.vert spectr) of
        True  -> Nothing
        False -> 
            case Space.overlap X compass (constellation $ System.vert spectr) of
                True  ->  Just 
                    $ Bool.bool W E -- Select : W | E
                    $ all ((<) (Space.pivot X (center compass)) . Space.pivot X) 
                        (constellation $ System.vert spectr) 
                False -> 
                    case Space.overlap Y compass (constellation $ System.vert spectr) of
                        True  -> Just 
                            $ Bool.bool S N -- Select : N | S
                            $ all ((<) (Space.pivot Y (center compass)) . Space.pivot Y) 
                                (constellation $ System.vert spectr)
                        False -> select 
                            (center compass) 
                            (constellation $ System.vert spectr)
                            -- Select : NE | SE | SW | NW

select :: Math.Point -> [Math.Point] -> Maybe Energy
select p ps = case all ((<) (Space.pivot X p) . Space.pivot X) ps of
    True  -> case all ((<) (Space.pivot Y p) . Space.pivot Y) ps of
        True  -> Just NE
        False -> Just SE
    False -> case all ((<) (Space.pivot Y p) . Space.pivot Y) ps of
        True  -> Just NW
        False -> Just SW

insert :: Spectrum -> Overline Spectrum -> Overline Spectrum
insert spectr struct = case direction spectr struct of
    Nothing -> struct { System.vert = append spectr (System.vert struct) }
    Just k  -> let
        f N  = case (k == N)  of True  -> (edge struct) N  >>= return . insert spectr
                                 False -> (edge struct) N
        f NE = case (k == NE) of True  -> (edge struct) NE >>= return . insert spectr
                                 False -> (edge struct) NE
        f E  = case (k == E)  of True  -> (edge struct) E  >>= return . insert spectr
                                 False -> (edge struct) E
        f SE = case (k == SE) of True  -> (edge struct) SE >>= return . insert spectr
                                 False -> (edge struct) SE
        f S  = case (k == S)  of True  -> (edge struct) S  >>= return . insert spectr
                                 False -> (edge struct) S
        f SW = case (k == SW) of True  -> (edge struct) SW >>= return . insert spectr
                                 False -> (edge struct) SW
        f W  = case (k == W)  of True  -> (edge struct) W  >>= return . insert spectr
                                 False -> (edge struct) W
        f NW = case (k == NW) of True  -> (edge struct) NW >>= return . insert spectr
                                 False -> (edge struct) NW
        in struct { edge = f }


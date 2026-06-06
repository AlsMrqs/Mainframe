module Struct.Mouse where
import Graphics.UI.GLUT
import Data.Bool    as Bool
import Struct.Space as Space

import Struct.Math  as Math
square = Math.fromIntegralPoint

data Click = Click
    { button :: MouseButton
    , state  :: KeyState
    , buffer :: Position } deriving (Show)

data Track = Track 
    { click    :: Maybe Click
    , position :: Position } deriving (Show)

difference :: Track -> (Math.Rad, Math.Rad)
difference (Track cli p) = maybe (0,0) (diff (toTup p) . toTup . buffer) $ cli
    where
    toTup (Position a b) = (fromIntegral a, fromIntegral b)
    diff (a,b) (c,d) = (a-c,b-d)

setClick :: Maybe Click -> Track -> Track
setClick x (Track _ p) = Track x p

setPosition :: Position -> Track -> Track
setPosition p (Track i _) = Track i p

-- Mouse Buffer
-- OnClick Buffer - LeftOff
-- OffClick (Difference) -> Rotate Object (MouseButton)
-- LeftButton (X,Y)
-- RightButton (Z)

-- instance Space.Coordinate Track where
--     coord axis (Track _ pos) = let (x,y,z) = pos
--         in case axis of
--             X -> x
--             Y -> y
--             Z -> z

-- lokat :: Position -> (Space.Coord, Space.Coord, Space.Coord)
-- lokat = \(Position x y) -> square (x,y,0)

-- check :: Click -> Track -> Bool
-- check (Click b s) = maybe False check' . input
--     where
--     check' = \m -> (but m == b) && (sta m == s)


module Struct.Space where
-- import Graphics.Rendering.OpenGL
-- import Graphics.UI.GLUT
import qualified System.Random as Random
import qualified Data.Bool as Bool (bool)
import qualified Data.List as List (foldl1', foldl')
 
type Point = (Coord, Coord, Coord)
type Coord = Double

class Coordinate a where coord :: Axis -> a -> Coord
data Axis = X | Y | Z

point :: Coordinate a => a -> Point
point k = (,,) (coord X k) (coord Y k) (coord Z k)

-- ??
overlap :: Coordinate a => Axis -> a -> [Point] -> Bool
overlap axis p ps = 
    any ((<=) (coord axis p) . pivot axis) ps &&
    any ((>=) (coord axis p) . pivot axis) ps 

pivot :: Axis -> Point  -> Coord
pivot axis (x,y,z) = case axis of
    X -> x
    Y -> y
    Z -> z

znear :: Point -> [Point] -> Point
znear = List.foldl' (\(x,y,z) (x',y',z') -> Bool.bool (x',y',z') (x,y,z) (z < z'))

zfar :: Point -> [Point] -> Point
zfar = List.foldl' (\(x,y,z) (x',y',z') -> Bool.bool (x',y',z') (x,y,z) (z > z'))

select :: (Coordinate a) => Axis -> (Coord -> Coord -> Bool) -> [a] -> a
select axis fn = List.foldl1' eval
    where
    eval a b = Bool.bool b a
        $ fn (coord axis a) (coord axis b)

generateCoord:: IO Coord
generateCoord = Random.newStdGen >>= return . fst . Random.randomR (-0.75,0.75)


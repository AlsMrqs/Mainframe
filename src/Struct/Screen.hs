module Struct.Screen where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Bool      as Bool (bool)
import Data.List      as List (foldl',concatMap,uncons)
import Struct.Manager as Manager
import Struct.System  as System 
import Struct.Space   as Space
import Struct.Math    as Math
import Struct.DBMS    as DBMS

type Flipside      = GLfloat
type RGB           = Color3
type Interactive a = System.Graph Element a

data Element       = Element
    { _RGB_ :: RGB Flipside
    , model :: PrimitiveMode  
    , ordin :: [(Space.Coord, Space.Coord, Space.Coord)]
    } deriving Show

instance Eq Element where
    (==) a b = (ax,ay,az) == (bx,by,bz)
        where
        (ax,ay,az) = (Space.coord X a, Space.coord Y a, Space.coord Z a)
        (bx,by,bz) = (Space.coord X b, Space.coord Y b, Space.coord Z b)

instance Ord Element where
    (<=) x y = case uncons (ordin x) of
        Nothing     -> False
        Just (p,ps) -> case uncons (ordin y) of
            Nothing     -> False
            Just (q,qs) -> let
                (xx,xy,xz) = (Space.znear p ps) 
                (yx,yy,yz) = (Space.znear q qs)
                in xz <= yz
    (>=) x y = case uncons (ordin x) of
        Nothing     -> False
        Just (p,ps) -> case uncons (ordin y) of
            Nothing     -> False
            Just (q,qs) -> let
                (xx,xy,xz) = (Space.znear p ps) 
                (yx,yy,yz) = (Space.znear q qs)
                in xz >= yz

instance Space.Coordinate Element where
    coord axis (Element _ _ []    ) = 0
    coord axis (Element _ _ (p:ps)) = 
        let (x,y,z) = Space.znear p ps 
        in case axis of 
            Space.X -> x
            Space.Y -> y
            Space.Z -> z


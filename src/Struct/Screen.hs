module Struct.Screen where
import Graphics.Rendering.OpenGL -- import Graphics.UI.GLUT
import qualified Data.List as List
import Struct.Graph   as Graph 
import Struct.Space   as Space

type Flipside      = GLfloat
type RGB           = Color3
type Interactive a = Graph.Graph Element a

data Element = Element
    { model :: PrimitiveMode
    , _RGB_ :: RGB Flipside
    , ordin ::[Space.Point] } deriving (Show) -- , Read)

instance Eq Element where
    (==) a b = (List.sort $ ordin a) == (List.sort $ ordin b)

instance Ord Element where
    (<=) x y = case List.uncons (ordin x) of
        Nothing     -> False
        Just (p,ps) -> case List.uncons (ordin y) of
            Nothing     -> False
            Just (q,qs) -> let
                (_,_,xz) = (Space.znear p ps) 
                (_,_,yz) = (Space.znear q qs)
                in xz <= yz
    (>=) x y = case List.uncons (ordin x) of
        Nothing     -> False
        Just (p,ps) -> case List.uncons (ordin y) of
            Nothing     -> False
            Just (q,qs) -> let
                (_,_,xz) = (Space.znear p ps) 
                (_,_,yz) = (Space.znear q qs)
                in xz >= yz

instance Space.Coordinate Element where
    coord _    (Element _ _ []    ) = 0
    coord axis (Element _ _ (p:ps)) = 
        let (x,y,z) = Space.znear p ps 
        in case axis of 
            Space.X -> x
            Space.Y -> y
            Space.Z -> z


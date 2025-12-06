module Engine.Element where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)

import Data.Bool

import Kernel.Core
import Engine.Math.Space as Math

data Element = Element
    { code :: String
    , fig  :: Graph Figure Position }

data Figure = Figure
    { mode     :: PrimitiveMode
    , theColor :: Color3 GLfloat
    , location :: [Point] } deriving Show

bitmapButton = Element
    { code = "bitmap.Button"
    , fig  = mouseOff }

mouseOff :: Graph Figure Position
mouseOff = let loc = [(0.5,0.5,0.5), (0.6,0.5,0.5), (0.6,0.4,0.5), (0.5,0.4,0.5)] in 
    Vertex
        { vert = Figure 
            { mode     = Polygon
            , theColor = Color3 0.5 0.5 (0.5 :: GLfloat)
            , location = loc }
        , edge = [\p -> bool Nothing (Just mouseOver) $ isInsidePolygon (positionToPoint p) loc] }

mouseOver :: Graph Figure Position
mouseOver = let loc = [(0.5,0.5,0.5), (0.6,0.5,0.5), (0.6,0.4,0.5), (0.5,0.4,0.5)] in
    Vertex
    { vert = Figure
        { mode     = Polygon
        , theColor = Color3 1 0.5 (0.5 :: GLfloat)
        , location = loc }
    , edge = [\p -> bool Nothing (Just mouseOff) $ isInsidePolygon (positionToPoint p) loc] }


positionToPoint :: Position -> Point
positionToPoint pos =
    let (Position x y) = pos
        x' = (+) (-1) $ (*) 2 (fromIntegral(x)/800)
        y' = (+) (1) $ (*) (-2) (fromIntegral(y)/600)
    in (x',y',0)


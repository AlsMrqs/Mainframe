module Struct.Engine where
import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)
import qualified Data.Map as Map
import Struct.Compass as Compass
import Struct.Program as Program
import Struct.Manager as Manager
import Struct.System  as System
import Struct.Screen  as Screen
import Struct.Space   as Space
import Struct.DBMS    as DBMS

import Struct.Math    as Math
online = Math.isInsidePolygon
square = Math.fromIntegralPoint

type Engine a = Compass.Compass a
type Entity   = Screen.Interactive Math.Point
type Program  = DBMS.DBMS (Engine Entity)

runtime :: DBMS.DBMS (Engine Entity)
runtime = DBMS.create (database)
    where
    database :: Engine Entity
    database = Compass.create button

constellation :: Screen.Element -> [(Space.Coord, Space.Coord, Space.Coord)]
constellation = Screen.ordin

coordinate :: [(Space.Coord, Space.Coord, Space.Coord)]
coordinate = [(-0.01,0.01,0),(0.01,0.01,0),(0.01,-0.01,0),(-0.01,-0.01,0)]

mouseOff :: Screen.Element
mouseOff = Screen.Element
    { Screen._RGB_ = Color3 0 0 (0.3 :: Screen.Flipside)
    , Screen.model = Polygon
    , Screen.ordin = coordinate }

mouseOver :: Screen.Element
mouseOver = Screen.Element
    { Screen._RGB_ = Color3 1 0 (0.3 :: Screen.Flipside)
    , Screen.model = Polygon
    , Screen.ordin = coordinate }

button :: Entity
button = ground
    where
    ground = System.Vertex mouseOff $ \(x,y,z) -> 
        case (x,y,z) `online` (constellation mouseOver) of -- [+] online
            True  -> Just overshot
            False -> Just ground

    overshot = System.Vertex mouseOver $ \(x,y,z) -> 
        case (x,y,z) `online` (constellation mouseOver) of -- [+] online
            True  -> Just overshot
            False -> Just ground


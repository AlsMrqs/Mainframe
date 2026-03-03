module Struct.Engine where
import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)
import qualified Data.Bool as Bool
import qualified Data.Map  as Map
import Struct.Compass as Compass
import Struct.Program as Program
import Struct.Manager as Manager
import Struct.System  as System
import Struct.Screen  as Screen
import Struct.Mouse   as Mouse 
import Struct.Space   as Space
import Struct.DBMS    as DBMS

import Struct.Math    as Math
online = Math.isInsidePolygon   -- [+]

type Program  = DBMS.DBMS (Engine Entity) 
type Engine a = Compass.Compass a -- Compass.Overline a
type Entity   = Screen.Interactive Mouse.Track

runtime :: DBMS.DBMS (Engine Entity)
runtime = DBMS.create (database) -- Empty -> insert (Entity)
    where
    database :: Engine Entity
    database = Compass.create button

constellation :: Screen.Element -> [(Space.Coord, Space.Coord, Space.Coord)]
constellation = Screen.ordin

coordinate :: [(Space.Coord, Space.Coord, Space.Coord)]
coordinate = [(-0.1,0.1,0),(0.1,0.1,0),(0.1,-0.1,0),(-0.1,-0.1,0)]

mouseOff :: Screen.Element
mouseOff = Screen.Element
    { Screen._RGB_ = Color3 0.6 0 (0.3 :: Screen.Flipside)
    , Screen.model = Polygon
    , Screen.ordin = coordinate }

mouseOver :: Screen.Element
mouseOver = Screen.Element
    { Screen._RGB_ = Color3 0.9 0 (0.3 :: Screen.Flipside)
    , Screen.model = Polygon
    , Screen.ordin = coordinate }

mouseClic :: Screen.Element
mouseClic = Screen.Element
    { Screen._RGB_ = Color3 0.9 0.9 (0.3 :: Screen.Flipside)
    , Screen.model = Polygon
    , Screen.ordin = coordinate }

button :: Entity
button = ground
    where
    ground = System.Vertex mouseOff $ \mouse -> 
        case (Space.point mouse) `online` (constellation mouseOver) of
            True  -> Just overshot
            False -> Just ground

    overshot = System.Vertex mouseOver $ \mouse -> 
        case (Space.point mouse) `online` (constellation mouseOver) of
            False -> Just ground
            True  -> 
                case Mouse.check (Mouse.Click LeftButton Down) mouse of
                    True  -> Just headshot
                    False -> Just overshot

    headshot = System.Vertex mouseClic $ \mouse -> 
        case (Space.point mouse) `online` (constellation mouseOver) of
            False -> Just ground
            True  -> 
                case Mouse.check (Mouse.Click LeftButton Down) mouse of
                    True  -> Just headshot
                    False -> Just overshot


module Program.Engine.Main where

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import Control.Concurrent.MVar
import Control.Concurrent

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Program.Engine.Element as Engine.Element
import Program.Engine.Control as Engine.Control
import Program.Engine.Render as Engine.Render

import Struct.Program as Program
import Struct.System as System
import Struct.Screen as Screen

root :: Program.System 
root = Program.Node
    { program = engine
    , caller  = Maybe.Nothing
    , link    = [] }

engine :: Program.Program
engine = Program.Program
    { name          = "engine"

    -- Controller
    , Program.keyboardMouse = Engine.Control.keyboardMouse
    , Program.mouse         = Engine.Control.mouse
    , Program.motion        = Engine.Control.motion
    , Program.passiveMotion = Engine.Control.passiveMotion

    -- Screen
    , Program.display       = Engine.Render.display
    , Program.interface     = Engine.Element.interface
    
    -- Database
    , terminal      = CLI False [] [[]] }


module Engine.Program where

import Control.Concurrent.MVar
import Graphics.Renderign.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import Data.Set as Set
import Data.Map as Map


{-Executable-} -- Program
data Program = Program
    { name          :: [Char]                                   
    -- Program Executable
    , call          :: Maybe Node -> IO ()

    -- Program Coltroller
    , keyboardMouse :: (Window, IORef Int) ->  MVar (Node) -> KeyboardMouseCallback 
    , mouse         :: MVar (Node) -> MouseButton -> KeyState -> Position -> IO () 
    , motion        :: MVar (Node) -> Position -> IO () 
    , passiveMotion :: MVar (Node) -> Position -> IO ()

    -- Program Screen | Design !!!!!!!!
    -- Implement (Z-Buffer) here!!
    , frame         :: [(GLfloat,GLfloat,GLfloat)]
    , polygons      :: Set.Set [(GLfloat,GLfloat,GLfloat)] 
        -- Write {Struct} to it!!!!!!
    , object        :: Map.Map (PrimitiveMode, [(GLfloat,GLfloat,GLfloat)]) String
    -- Element!!!
    -- , element       :: Map.Map String Element
    sketch          :: Map.Map String Element

    -- DataBase -> Write it better $ Generic Database
    -- Sprites -> Database !!
    , terminal      :: CLI
    , sprites       :: Map.Map [Char] [[(GLfloat,GLfloat,GLfloat)]] } -- [Polygon]

data CLI = CLI
    { active  :: Bool
    , inbox   :: [Char]
    , history :: [[Char]] } deriving Show

turnOn :: CLI -> CLI
turnOn cli = cli { active = True }

turnOff :: CLI -> CLI
turnOff cli = cli { active = False }

saveInbox :: CLI -> CLI
saveInbox cli = CLI (active cli) [] $
    if (inbox cli) == []
        then history cli 
        else history cli ++ [inbox cli]


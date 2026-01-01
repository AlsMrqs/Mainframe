module Engine.Struct where
    -- todo: exports -> Graphics.Rendering.OpenGL
    -- Maybe unecessary

import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT hiding (Program)
import Control.Concurrent.MVar

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.IORef

import Engine.Element

{-Network-} -- Graph
-- data Node = Node
--     { program :: Program
--     , caller  :: Maybe Node
--     , link    :: [Node] }
-- 
-- access :: String -> Node ->  Maybe Node
-- access str node = List.find ((==) str . name . program) . link $ node

-- {-Executable-} -- Program
-- data Program = Program
--     { name          :: [Char]                                   
--     -- Program Executable
--     , call          :: Maybe Node -> IO ()
-- 
--     -- Program Coltroller
--     , keyboardMouse :: (Window, IORef Int) ->  MVar (Node) -> KeyboardMouseCallback 
--     , mouse         :: MVar (Node) -> MouseButton -> KeyState -> Position -> IO () 
--     , motion        :: MVar (Node) -> Position -> IO () 
--     , passiveMotion :: MVar (Node) -> Position -> IO ()
-- 
--     -- Program Screen | Design !!!!!!!!
--     -- Implement (Z-Buffer) here!!
--     , frame         :: [(GLfloat,GLfloat,GLfloat)]
--     , polygons      :: Set.Set [(GLfloat,GLfloat,GLfloat)] 
--         -- Write {Struct} to it!!!!!!
--     , object        :: Map.Map (PrimitiveMode, [(GLfloat,GLfloat,GLfloat)]) String
--     -- Element!!!
--     , element       :: Map.Map String Element
-- 
--     -- DataBase -> Write it better $ Generic Database
--     -- Sprites -> Database !!
--     , terminal      :: CLI
--     , sprites       :: Map.Map [Char] [[(GLfloat,GLfloat,GLfloat)]] } -- [Polygon]

-- data CLI = CLI
--     { active  :: Bool
--     , inbox   :: [Char]
--     , history :: [[Char]] } deriving Show
-- 
-- turnOn :: CLI -> CLI
-- turnOn cli = cli { active = True }
-- 
-- turnOff :: CLI -> CLI
-- turnOff cli = cli { active = False }
-- 
-- saveInbox :: CLI -> CLI
-- saveInbox cli = CLI (active cli) [] $
--     if (inbox cli) == []
--         then history cli 
--         else history cli ++ [inbox cli]

-- todo:
-- newtype Hub a = Hub (Map.Map [Char] a)


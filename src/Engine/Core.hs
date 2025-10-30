module Engine.Core where
    -- todo: exports -> Graphics.Rendering.OpenGL
    -- Maybe unecessary

import Graphics.UI.GLUT hiding (Program)
import Control.Concurrent.MVar

{-Network-} -- Graph
data Node = Node
    { program :: Program
    , caller  :: Maybe Node
    , link    :: [Node] }

{-Executable-} -- Program
data Program = Program
    { name :: [Char]
    , keyboardMouse :: MVar (Node) -> KeyboardMouseCallback 
    , mouse :: MVar (Node) -> MouseButton -> KeyState -> Position -> IO () 
    , motion :: MVar (Node) -> Position -> IO () 
    , frame :: [(GLfloat,GLfloat,GLfloat)] }


module Interface where

import qualified Graphics.UI.GLUT as GLUT

class Interface p where
    display       :: p -> GLUT.DisplayCallback
    keyboardMouse :: p -> GLUT.KeyboardMouseCallback
    mouse         :: p -> GLUT.MouseCallback
    motion        :: p -> GLUT.MotionCallback
    passiveMotion :: p -> GLUT.MotionCallback



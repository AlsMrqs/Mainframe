module Engine.Control where

import Control.Monad.IO.Class
import Graphics.UI.GLUT

-- Control --
keyboardMouse :: MonadIO m => Key -> KeyState -> p1 -> p2 -> m ()
keyboardMouse key keyState _ pos = do
    case (key,keyState) of
        (Char 'q', Down) -> leaveMainLoop
        _                -> return ()


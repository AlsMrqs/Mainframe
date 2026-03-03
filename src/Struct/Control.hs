module Struct.Control where
import Graphics.Rendering.OpenGL hiding (Program)
import Graphics.UI.GLUT          hiding (Program)
import Control.Concurrent.MVar
import Control.Concurrent
-- import Csound.IO
-- import Csound.Base
import qualified Struct.BitMap as BitMap

mouse :: MVar (BitMap.BitMap) -> MouseCallback
mouse _root _button _keyState _pos = do
    (Size w h) <- get windowSize

    _bitmap <- takeMVar _root
    let newBit = BitMap.setBit _bitmap _pos (Size w h)
    putMVar _root $ BitMap.insertBit newBit _bitmap

motion :: MVar (BitMap.BitMap) -> MotionCallback
motion _root _pos = return ()
    
keyboardMouse :: MVar (BitMap.BitMap) -> KeyboardMouseCallback
keyboardMouse _root key keyState _mod pos = return ()


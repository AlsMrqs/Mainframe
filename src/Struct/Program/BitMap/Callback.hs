module Struct.Program.BitMap.Callback 
    ( module BitMap
    , display 
    , mouse ) 
where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
-- import qualified Control.Concurrent.MVar as MVar
import Struct.Program.BitMap.BitMap as BitMap
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- 
import qualified Struct.Screen as Screen
-- import qualified Struct.Space  as Space
import qualified Struct.Render as Render

-- display :: MVar.MVar (BitMap.BitMap) -> GLUT.DisplayCallback
display :: BitMap.BitMap -> GLUT.DisplayCallback
display _bitmap = do
    GLUT.clear [GLUT.ColorBuffer]

    -- _bitmap <- MVar.readMVar _root
    GLUT.color $ currentRGB _bitmap
    let elementList = Set.toList $ bitSet _bitmap

    mapM_ (\el -> OpenGL.renderPrimitive  (Screen.model el) 
        $ Render.flash (Render.constellation el)) elementList

    -- GLUT.swapBuffers
    -- GLUT.postRedisplay Nothing
-- 
-- keyboardMouse :: MVar.MVar (BitMap.BitMap) -> GLUT.KeyboardMouseCallback
-- keyboardMouse _root _key _keyState _mod _pos = return () -- do
--     -- case (_key, _keyState) of
--     --     (GLUT.Char '\r', GLUT.Down) -> do
--     --         putStrLn "Selected char: " 
--     --         char <- getChar
--     --         _data <- loadFont baseFont
--     --         case _data of
--     --             Nothing -> do
--     --                 MVar.readMVar _root 
--     --                     >>= return . Map.singleton char .  map bitToPix . extract
--     --                     >>= flip saveFont baseFont
--     --                     >>= putStrLn 
--     --             Just k  -> print (read k :: Map.Map Char [BitMap.Pix])
--     --     (GLUT.Char 'l', GLUT.Down) -> do
--     --         putStrLn "Element to load:"
--     --         char <- getChar
--     --         putStrLn $ "Char " ++ show char
--     --         loadFont baseFont >>= print
--     --     _ -> return ()
-- 
-- mouse :: GLUT.MouseButton -> GLUT.KeyState -> GLUT.Position -> BitMap.BitMap -> IO BitMap.BitMap
--
type CallbackMouse = GLUT.Size ->
    GLUT.MouseButton -> GLUT.KeyState -> GLUT.Position -> BitMap.BitMap -> BitMap.BitMap

mouse :: CallbackMouse
mouse _gs _button _keyState _pos _bitmap =
    let newBit = BitMap.setBit _bitmap _pos _gs
    in BitMap.insertBit newBit _bitmap


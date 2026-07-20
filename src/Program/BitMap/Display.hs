module Program.BitMap.Display
    ( module BitMap
    , display
    , mouse ) 
where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Program.BitMap.BitMap as BitMap
import qualified Data.Set as Set
import qualified Screen as Screen
import qualified Render as Render

display :: BitMap.BitMap -> GLUT.DisplayCallback
display _bitmap = do
    GLUT.clear [GLUT.ColorBuffer]

    GLUT.color $ currentRGB _bitmap
    let elementList = Set.toList $ bitSet _bitmap

    mapM_ (\el -> OpenGL.renderPrimitive  (Screen.model el) 
        $ Render.flash (Render.constellation el)) elementList

type CallbackMouse = GLUT.Size ->
    GLUT.MouseButton -> GLUT.KeyState -> GLUT.Position -> BitMap.BitMap -> BitMap.BitMap

mouse :: CallbackMouse
mouse _gs _button _keyState _pos _bitmap =
    let newBit = BitMap.setBit _bitmap _pos _gs
    in BitMap.insertBit newBit _bitmap


module Struct.Program.BitMap.BitMap where
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT          as GLUT
-- import qualified Control.Concurrent.MVar   as MVar
import qualified System.IO.Error as Error
import qualified System.IO       as IO
import qualified Data.Bool as Bool
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified GHC.Int as GHC.Int
import qualified Struct.Screen as Screen
import qualified Struct.Space  as Space
-- import qualified Struct.Render as Render
-- import qualified Struct.Font   as Font

type Bit = Screen.Element
data Pix = Pix (Screen.RGB Screen.Flipside) [Space.Point] 
    deriving (Show, Read)

pixToBit :: Pix -> Bit
pixToBit (Pix rgb' coord) = Screen.Element (GLUT.Polygon) (rgb') (coord)

bitToPix :: Bit -> Pix
bitToPix (Screen.Element _ rgb' coord) = Pix rgb' coord

data Index = Index GHC.Int.Int32 GHC.Int.Int32 
    deriving Show

data Pixel = Pixel
    { width  :: Double 
    , height :: Double } deriving (Show)

_Num :: (Integral a, Num b) => a -> b
_Num = fromIntegral

toPixel :: GLUT.Size -> Pixel
toPixel (GLUT.Size x y) = Pixel ( 2.0/(_Num x) ) ( 2.0/(_Num y) )

type BitSize = GHC.Int.Int32

data BitMap = BitMap -- initialize with Screen (Size)
    { bitSet     :: Set.Set Bit
    , bitSize    :: BitSize 
    , pixel      :: Pixel
    , currentRGB :: OpenGL.Color3 OpenGL.GLfloat } -- deriving Show

instance Show BitMap where 
    show bitm = "BitSet: " ++ (show $ bitSet bitm)
        ++ "\nBitSize: " ++ (show $ bitSize bitm)
        ++ "\nPixel: " ++ (show $ pixel bitm)
        ++ "\nCurrentRGB: " ++ (show $ currentRGB bitm)

rgb :: a -> a -> a -> GLUT.Color3 a
rgb = OpenGL.Color3

bitmap :: GLUT.Size -> BitMap
bitmap glts = BitMap Set.empty 15 (toPixel glts) (rgb 1 0 1)

setColor :: OpenGL.Color3 OpenGL.GLfloat -> BitMap -> BitMap
setColor clr bitm = bitm { currentRGB = clr }

-- Tooling

extract :: BitMap -> [Bit]
extract = Set.toList . bitSet 

baseFont :: IO.FilePath
baseFont = "./Base.fnt"

saveFont :: Map.Map Char [Pix] -> IO.FilePath -> IO String
saveFont k fp = (Error.tryIOError . writeFile fp . show $ Map.toList k)
    >>= return . either show (const "Sucess!")

loadFont :: IO.FilePath -> IO (Maybe String)
loadFont fp = (Error.tryIOError $ readFile fp)
    >>= return . either (const Nothing) Just

resize :: Bool -> BitMap -> BitMap -- resize (Bit) !!!
resize _dir bitm = bitm { bitSize = (bitSize bitm) + step }
    where
    step = Bool.bool (-2) 2 _dir

-- data Zoom = In | Out
-- 
-- zoom :: Zoom -> Bit -> Bit
-- zoom = 

-- BitMap.display
insertBit :: Bit -> BitMap -> BitMap
insertBit bit bitm = bitm { bitSet = Set.insert bit currentBitSet } -- Data.Set
    where
    currentBitSet = bitSet bitm -- Data.Set

index :: BitSize -> GLUT.Position -> Index
index bs (GLUT.Position x y) = Index (div x bs) (div y bs)

-- setBit' :: GLUT.Position -> BitMap -> IO () -- Bit
-- setBit' (GLUT.Position x y) bitm = return () -- newBit

setBit :: BitMap -> GLUT.Position -> GLUT.Size -> Bit
setBit bitm (GLUT.Position x y) (GLUT.Size _width _height) = newBit
    where
    (Index w h) = index (bitSize bitm) (GLUT.Position x y)
    pixelSizeX = (*) (fromIntegral $ bitSize bitm) (2 / fromIntegral(_width))
    pixelSizeY = (*) (fromIntegral $ bitSize bitm) (2 / fromIntegral(_height))

    pixelPositionX = (+) (-1) $ pixelSizeX * fromIntegral(w)
    pixelPositionY = (+) 1 . negate $ pixelSizeY * fromIntegral(h)

    newBit = Screen.Element
        { Screen.model = OpenGL.Polygon
        , Screen._RGB_ = currentRGB bitm
        , Screen.ordin = _coords }

    x' = pixelPositionX
    y' = pixelPositionY
    _coords = 
        [ (x',y',0)
        , (x'+pixelSizeX,y',0)
        , (x'+pixelSizeX,y'-pixelSizeY,0)
        , (x',y'-pixelSizeY,0) ]

setPolygon :: BitSize -> (Space.Coord, Space.Coord) -> [Space.Point]
setPolygon bs (x,y) = [(x,y,0),(x+bs',y,0),(x+bs',y-bs',0),(x,y-bs',0)]
    where
    bs' = fromIntegral bs


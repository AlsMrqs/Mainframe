module Struct.Program where
import qualified Graphics.UI.GLUT as GLUT
import qualified Struct.Program.Shell.Callback as Shell
import qualified Struct.Program.BitMap.Callback as BitMap
import qualified Struct.Manager as Manager
import qualified Struct.Graph as Graph
import qualified Data.List as List (elem)
import qualified Data.Bool as Bool (bool)
import qualified Data.Maybe as Maybe (isNothing)

data Program = Program
    { shell  :: Maybe Shell.Shell
    , bitmap :: Maybe BitMap.BitMap 
    } deriving Show

type ProgName = String

programs :: [ProgName]
programs = ["shell","bitmap"]

close :: String -> (Program -> Program)
close str = case str of
    "bitmap" -> \prg -> prg { bitmap = Nothing }
    _        -> id


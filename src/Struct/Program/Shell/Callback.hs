module Struct.Program.Shell.Callback (module Shell) where
-- import qualified Graphics.UI.GLUT as GLUT
-- import qualified Control.Concurrent.MVar as MVar
import Struct.Program.Shell.Shell as Shell

-- display :: MVar.MVar (Shell.Shell) -> GLUT.DisplayCallback
-- display mvar = return () 
-- 
-- keyboardMouse :: MVar.MVar (Shell.Shell) -> GLUT.KeyboardMouseCallback
-- keyboardMouse mvar _key _keyState _ _ = do
--     case (_key, _keyState) of
--         (GLUT.Char '\r', GLUT.Down) -> putStrLn "Start was pressed!"
--         (GLUT.Char k   , GLUT.Down) -> return ()
--         _ -> return ()
-- 
-- mouse :: MVar.MVar (Shell.Shell) -> GLUT.MouseCallback
-- mouse _ _ _ _ = return ()


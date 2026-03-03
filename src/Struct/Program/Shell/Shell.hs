module Struct.Program.Shell.Shell where
import qualified Data.List as List (init) -- ,intercalate)
import qualified GHC.Int as GHC.Int

data ShellSize = ShellSize
    { widht  :: GHC.Int.Int32
    , height :: GHC.Int.Int32 } deriving Show

data Shell = Shell
    { size    :: ShellSize
    , history :: [String]
    , inbox   :: [Char] } deriving (Show)

main :: IO Shell
main = return newShell

newShell :: Shell
newShell = Shell (ShellSize (3+12) (5+12)) [] []

insertInbox :: Char -> Shell -> Shell
insertInbox c shell = shell { inbox = newInbox }
    where
    newInbox = (inbox shell) ++ [c]

eraseInbox :: Shell -> Shell
eraseInbox shell = shell { inbox = newInbox }
    where
    newInbox = if null (inbox shell) then [] else List.init (inbox shell)

deleteInbox :: Shell -> Shell
deleteInbox shell = shell { inbox = [] }

updateHistory :: Shell -> Shell
updateHistory shell = shell { history = newHistory }
    where
    newHistory = (inbox shell) : (history shell)


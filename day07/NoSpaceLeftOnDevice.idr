-- Day 7: No Space Left On Device

module NoSpaceLeftOnDevice
import System
import System.File
import Data.Fin
import Data.String
import Data.List
import Data.String.Parser
import Debug.Trace


{-- Parsing --}

data FileDir = File Nat String | Dir String

implementation Show FileDir where
    show (File size name) = "\{name} (file, size=\{show size})"
    show (Dir name) = "(\{name} (dir)"

data Command = CDHome
             | CDParent
             | CD String 
             | LS (List FileDir)

implementation Show Command where
    show CDHome = "cd /"
    show CDParent = "cd .."
    show (CD dir) = "cd \{dir}"
    show (LS xs) = "ls \{show xs}"

InputType : Type
InputType = List Command

parseFilDir : Parser FileDir
parseFilDir = token "dir" *> Dir <$> takeWhile (/= '\n')
          <|> File <$> lexeme natural <*> takeWhile (/= '\n')

parseFileDirs : Parser $ List FileDir
parseFileDirs = sepBy parseFilDir $ char '\n'

parseCommand : Parser Command
parseCommand = string "$ cd .." $> CDParent
           <|> string "$ cd /"  $> CDHome
           <|> string "$ ls\n"  *> LS <$> parseFileDirs
           <|> string "$ cd "   *> CD <$> takeWhile (/= '\n')

parseInput : Parser InputType
parseInput = sepBy parseCommand $ char '\n'

{-- Solution --}

Path : Type
Path = List String

mutual
    record Directory where
        constructor MkDir
        name : String
        children : List FileSys

    data FileSys : Type where
        Dir'  : (dir : Directory) -> FileSys
        File' : (name : String) -> (size : Nat) -> FileSys

    partial
    implementation Show Directory where
        show (MkDir name children) = "Dir (\{name}, children=\{show children})"

    partial
    implementation Show FileSys where
        show (Dir' root) = show root
        show (File' name size) = "File (\{name}, size=\{show size})"

addChild : (child : FileSys) -> (fs : FileSys) -> FileSys
addChild child (Dir' dir) = Dir' $ { children $= (child ::) } dir
addChild _ fs = fs

isDir : (name : String) -> FileSys -> Bool
isDir name (Dir' dir) = dir.name == name
isDir _ _ = False

addFileSys : (path : Path) -> (child : FileSys) -> (fs : FileSys) -> FileSys
addFileSys [] child fs = addChild child fs
addFileSys (p :: path') child (Dir' root) =
    let
        Just idx = findIndex (isDir p) root.children | Nothing => (Dir' root)
        (start, rest) = splitAt (finToNat idx) root.children
        elem = index' root.children idx
    in
        Dir' $ { children := start ++ [addFileSys path' child elem] ++ drop 1 rest } root
addFileSys _ _ fs = fs

toFileSys : FileDir -> FileSys
toFileSys (File size name) = File' name size
toFileSys (Dir name) = Dir' (MkDir name [])

evalCommands : (cmds : List Command) -> (path : Path) -> (fs : FileSys) -> FileSys
evalCommands [] _ fs = fs
evalCommands (cmd :: cmds) path fs = case cmd of
    CDHome => evalCommands cmds [] fs
    CDParent => evalCommands cmds (drop 1 path) fs
    (CD dir) => evalCommands cmds (dir :: path) fs
    (LS elems) =>  
        let
            fs' = (foldl (flip $ addFileSys (reverse path)) fs . map toFileSys) elems
        in
            evalCommands cmds path fs'

getDirs : (fs : FileSys) -> List FileSys
getDirs (File' _ _) = []
getDirs (Dir' root) = Dir' root :: (concat . map getDirs) root.children

countTotalSize : (fs : FileSys) -> Nat
countTotalSize (File' _ size) = size
countTotalSize (Dir' dir) = (sum . map countTotalSize) dir.children

getTotalSizeLt : Nat -> FileSys -> Nat
getTotalSizeLt n = sum . filter ( <= n) . map countTotalSize . getDirs

getMinToDelete : (requiredSpace: Nat) -> FileSys -> Maybe Nat
getMinToDelete requiredSpace = find ( >= requiredSpace) . sort . map countTotalSize . getDirs

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let fs = evalCommands input [] (Dir' (MkDir "/" []))
    printLn $ getTotalSizeLt 100000 fs
    printLn $ getMinToDelete (minus 30000000 $ minus 70000000 $ countTotalSize fs) fs

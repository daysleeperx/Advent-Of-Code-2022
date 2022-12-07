-- Day 7: No Space Left On Device

module NoSpaceLeftOnDevice
import System
import System.File
import Data.String
import Data.List
import Data.String.Parser
import Debug.Trace


{-- Parsing --}

data FileDir = File Nat String | Dir String

implementation Show FileDir where
    show (File size name) = "\{name} (file, size=\{show size})"
    show (Dir name) = "(\{name} (dir)"

data Command = CD String 
             | CDParent 
             | CDHome 
             | LS (List FileDir)

implementation Show Command where
    show (CD dir) = "cd \{dir}"
    show CDParent = "cd .."
    show CDHome = "cd /"
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

main : IO ()
main = do
    args <- getArgs
    -- let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile "input.txt" | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    putStr $ (unlines . map show) input

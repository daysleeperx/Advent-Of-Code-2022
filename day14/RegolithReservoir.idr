-- Day 14: Regolith Reservoir

module RegolithReservoir
import System
import System.File
import Data.List
import Data.String.Parser
import Data.SortedSet
import Debug.Trace


Coord : Type
Coord = (Integer, Integer)

Trace : Type
Trace = List Coord

InputType : Type
InputType = List Trace

parseCoord : Parser Coord
parseCoord = pure (,) <*> (integer <* char ',') <*> integer

parseTrace : Parser Trace
parseTrace = sepBy parseCoord $ string " -> "

parseInput : Parser InputType
parseInput = sepBy parseTrace $ char '\n'

testSet : SortedSet Coord
testSet = empty

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn input
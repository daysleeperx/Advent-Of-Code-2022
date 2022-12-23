-- Day 15: BeaconExclusionZone

module BeaconExclusionZone
import System
import System.File
import Data.List
import Data.String.Parser
import Debug.Trace


InputType : Type
InputType = ?InputType_rhs

parseInput : Parser InputType
parseInput = ?parseInput_rhs

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    pure ()
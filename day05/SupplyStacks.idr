-- Day 5: Supply Stacks

module SupplyStacks
import System
import System.File
import Data.List
import Data.Vect
import Data.String.Parser
import Debug.Trace

-- "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"


Crate : Type
Crate = Maybe Char

CargoRow : Type
CargoRow = List Crate

Cargo : Type
Cargo = List CargoRow

InputType : Type
InputType = ?InputType_rhs

parseCrate : Parser Crate
parseCrate = Just <$> (char '[' *> letter <* char ']') 
    <|> ntimes 3 space $> Nothing

parseRow : Parser CargoRow
parseRow = many parseCrate

parseCargo : Parser Cargo
parseCargo = sepBy parseRow $ char '\n'

parseInput : Parser InputType
parseInput = ?parseInput_rhs

main : IO ()
main = do
    args <- getArgs
    -- let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile "input.txt" | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    pure ()

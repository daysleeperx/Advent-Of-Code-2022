-- Day 3: Rucksack Reorganization

module RucksackReorganization
import System
import System.File
import Data.Nat
import Data.List
import Data.Stream
import Data.String.Parser


Item : Type
Item = Char

Rucksack : Type
Rucksack = List Item

InputType : Type
InputType = List Rucksack

parseRucksack : Parser Rucksack
parseRucksack = many letter

parseInput : Parser InputType
parseInput = sepBy parseRucksack $ char '\n'

priority : Char -> Int
priority ch with (isLower ch)
    _ | False = ord ch - 38
    _ | True = ord ch - 96

splitRucksack : Rucksack -> (Rucksack, Rucksack)
splitRucksack xs = splitAt (divNatNZ (length xs) 2 SIsNonZero) xs

inBothCompartments : Rucksack -> Rucksack
inBothCompartments = nub . uncurry intersect . splitRucksack

totalScore : InputType -> Int
totalScore = sum . map priority . concat . map inBothCompartments

chunks : Nat -> List a -> List (List a)
chunks n = takeBefore isNil . unfoldr (splitAt n)

totalScore' : InputType -> Int
totalScore' = sum . map priority . concat . map (nub . intersectAll) . chunks 3

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ totalScore input
    printLn $ totalScore' input

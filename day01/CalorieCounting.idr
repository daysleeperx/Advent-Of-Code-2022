-- Day 1: Calorie Counting

module CalorieCounting
import System
import System.File
import Data.List
import Data.List1
import Data.String.Parser


Calories : Type
Calories = List1 Nat

InputType : Type
InputType = List1 Calories

parseCalories : Parser Calories
parseCalories = sepBy1 natural $ char '\n'

parseInput : Parser InputType
parseInput = sepBy1 parseCalories spaces

mostCalories : InputType -> Nat
mostCalories = foldl max Z . map sum

topThreeElves : InputType -> List Nat
topThreeElves = take 3 . sortBy (flip compare) . forget . map sum

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    Right symbols <- readFile file | Left error => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ mostCalories input
    printLn $ sum $ topThreeElves input

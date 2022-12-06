-- Day 6: Tuning Trouble

module TuningTrouble
import System
import System.File
import Data.Fin
import Data.List
import Data.Stream
import Data.String.Parser


InputType : Type
InputType = List Char

parseInput : Parser InputType
parseInput = many letter

windowed : Nat -> List a -> List $ List a
windowed n = takeBefore isNil . unfoldr (\xs => (take n xs, drop 1 xs))

findPacketStart : (xs : List (List Char)) -> Maybe (Fin (length xs))
findPacketStart = findIndex (\xs => nub xs == xs)

countCharsToStart : Nat -> List Char -> Maybe Nat
countCharsToStart offset xs = findPacketStart (windowed offset xs) >>= pure . ( + offset) . finToNat

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let Just chars = countCharsToStart 4 input | Nothing => printLn "Not start index found"
    let Just chars' = countCharsToStart 14 input | Nothing => printLn "Not start index found"
    printLn chars
    printLn chars'

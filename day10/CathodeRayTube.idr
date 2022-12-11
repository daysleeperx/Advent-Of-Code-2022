-- Day 10: Cathode-Ray Tube

module CathodeRayTube
import System
import System.File
import Data.List
import Data.Stream
import Data.String
import Data.String.Parser
import Debug.Trace


data Instruction = Noop | Addx Integer

implementation Show Instruction where
    show Noop = "Noop"
    show (Addx x) = "Add \{show x}"

parseInstruction : Parser Instruction
parseInstruction = string "noop" $> Noop 
                <|> Addx <$> (token "addx" *> integer)

InputType : Type
InputType = List Instruction

parseInput : Parser InputType
parseInput = sepBy parseInstruction $ char '\n'

CPU : Type
CPU = (Integer, Integer)

eval : List Instruction -> List CPU
eval = eval' (1, 1) 
    where
        eval' : CPU -> List Instruction -> List CPU
        eval' cpu [] = []
        eval' (cycle, x) (instr :: xs) = case instr of
            Noop => (cycle, x) :: eval' (cycle + 1, x) xs
            Addx y => (cycle, x) :: (cycle + 1, x) :: eval' (cycle + 2, x + y) xs

getSumOfCycles : List CPU -> Integer
getSumOfCycles = sum . map (uncurry (*)) . filter (( `mod` 40 == 0). ( + 20) . fst)

chunks : Nat -> List a -> List (List a)
chunks n = takeBefore isNil . unfoldr (splitAt n)

toPixel : CPU -> Char
toPixel (cycle, x) = if abs (((cycle - 1) `mod` 40) - x) < 2 then '#' else '.'

getCRTRows : List CPU -> List $ List Char
getCRTRows = chunks 40 . map toPixel

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ (getSumOfCycles . eval) input
    (putStr . unlines . map pack . getCRTRows . eval) input

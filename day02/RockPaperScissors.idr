-- Day 2: Rock Paper Scissors

module RockPaperScissors
import System
import System.File
import Data.Nat
import Data.List
import Data.List1
import Data.Stream
import Data.String.Parser
import Debug.Trace


data Hand = Rock | Paper | Scissors

implementation Show Hand where
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

implementation Eq Hand where
    (==) x y = show x == show y

implementation Ord Hand where
    compare x y with (x == y)
        compare x y | True = EQ
        compare Rock Paper | _ = LT
        compare Paper Scissors | _ = LT
        compare Scissors Rock | _ = LT
        _ | _ = GT

Round : Type
Round = (Hand, Hand)

InputType : Type
InputType = List Round

parseHand : Parser Hand
parseHand = (token "A" <|> token "X") $> Rock
    <|> (token "B" <|> token "Y") $> Paper
    <|> (token "C" <|> token "Z") $> Scissors

parseRound : Parser Round
parseRound = (,) <$> parseHand <*> parseHand

parseInput : Parser InputType
parseInput = many parseRound

scoreHand : Hand -> Nat
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

scoreRound : Round -> Nat
scoreRound (x, y) with (compare x y)
  _ | LT = scoreHand y + 6
  _ | EQ = scoreHand y + 3
  _ | GT = scoreHand y + 0

totalScore : InputType -> Nat
totalScore = sum . map scoreRound

looseWin : Nat -> Hand
looseWin idx = index idx $ cycle [Rock, Paper, Scissors]

scoreRound' : Round -> Nat
scoreRound' (x, y) = (scoreRound . (x, ) . looseWin . choose) y
    where
        choose : Hand -> Nat
        choose Rock = S $ scoreHand x
        choose Paper with (scoreHand x)
          _ | (S k) = k
          _ | _ = Z
        choose Scissors = scoreHand x


totalScore' : InputType -> Nat
totalScore' = sum . map scoreRound'

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ totalScore input
    printLn $ totalScore' input

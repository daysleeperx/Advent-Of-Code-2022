-- Day 9: Rope Bridge

module RopeBridge
import System
import System.File
import Data.List
import Data.List1
import Data.String.Parser
import Debug.Trace


{--  Parsing --}

data Motion = Right | Left | Up | Down 

implementation Show Motion where
    show Right = "Right"
    show Left = "Left"
    show Up = "Up"
    show Down = "Down"

InputType : Type
InputType = List Motion

parseMotion : Parser $ List Motion
parseMotion = lexeme (char 'R') *> (flip replicate) Right <$> natural
          <|> lexeme (char 'L') *> (flip replicate) Left <$> natural
          <|> lexeme (char 'U') *> (flip replicate) Up <$> natural
          <|> lexeme (char 'D') *> (flip replicate) Down <$> natural

parseInput : Parser InputType
parseInput = concat <$> (sepBy parseMotion $ char '\n')

{-- Solution --}

Point : Type
Point = (Int, Int)

chebyshevDistance : Point -> Point -> Int
chebyshevDistance (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

sign : Int -> Int
sign x with (compare x 0)
    _ | LT = -1
    _ | EQ = 0
    _ | GT = 1

moveHead : Motion -> Point -> Point
moveHead Right (x, y) = (x + 1, y)
moveHead Left (x, y) = (x - 1, y)
moveHead Up (x, y) = (x, y + 1)
moveHead Down (x, y) = (x, y - 1)

moveTail : (tail : Point) -> (head : Point) -> Point
moveTail (x1, y1) (x2, y2) with (chebyshevDistance (x1, y1) (x2, y2) > 1)
    _ | False = (x1, y1)
    _ | True = 
        let
            dx = sign (x2 - x1)
            dy = sign (y2 - y1)
        in
            (x1 + dx, y1 + dy)

rope : (len : Nat) -> List1 Point
rope Z = rope 1
rope (S n) = (0, 0) ::: replicate n (0, 0)

updateTail : (head : Point) -> List Point -> List Point
updateTail head [] = []
updateTail head (tail :: xs) = 
    let 
        head' = moveTail tail head 
    in 
        head' :: updateTail head' xs

follow : (len : Nat) -> (motions : List Motion) -> (List1 Point, List Point)
follow len = foldl move (rope len, [])
    where 
        move : (List1 Point, List Point) -> Motion -> (List1 Point, List Point)
        move (rope, visited) m =
            let
                h' = moveHead m (head rope)
                tail' = updateTail h' (tail rope)
                rope' = (h' ::: tail')
            in
                (rope', last rope' :: visited)

countVisited : Nat -> List Motion -> Nat
countVisited len = length . nub . snd . follow len

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (motions, _) = parse parseInput symbols | Left error => printLn error
    printLn $ countVisited 2 motions
    printLn $ countVisited 10 motions
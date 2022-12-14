-- Day 12: Hill Climbing Algorithm

module HillClimbingAlgorithm
import System
import System.File
import Data.List
import Data.String
import Data.Stream
import Data.String.Parser
import Data.SortedMap
import Debug.Trace


Point : Type
Point = ((Int, Int), Char)

Graph : Type
Graph = SortedMap Point (List Point)

interface Queue (queue : Type -> Type) where
    empty : queue a
    push : a -> queue a -> queue a
    pop : queue a -> Maybe (Pair a (queue a))

data ListPair : Type -> Type where
    LP : (back : List a) -> (front : List a) -> ListPair a

implementation Queue ListPair where
    empty = LP [] []
    push x (LP back front) = LP (x :: back) front
    pop (LP [] []) = Nothing
    pop (LP back []) = pop $ LP [] (reverse back)
    pop (LP back (x :: xs)) = Just (x, LP back xs)

parseRows : Parser (List $ List Char)
parseRows = sepBy (many letter) $ char '\n'

indices : Nat -> List Int
indices Z = []
indices (S len) = [0..(cast len)]

zipRow : List Char -> List (Int, Char)
zipRow row = zip (indices $ length row) row

mapRowCoords : (Int, List (Int, Char)) -> List (Int, (Int, Char))
mapRowCoords (row, cols) = map ((row, )) cols

mapToCoords : List (List Char) -> List (Int, (Int, Char))
mapToCoords xs = concatMap mapRowCoords $ zip (indices $ length xs) $ map zipRow xs

toPoint : (Int, (Int, Char)) -> Point
toPoint (row, (col, val)) = ((row, col), val)

coordsToPoints : List (Int, (Int, Char)) -> List Point
coordsToPoints = map toPoint

deltas : List (Int, Int)
deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]

dirs : Point -> List (Int, Int)
dirs ((row, col), _) = map (\(dy, dx) => (row + dy, col + dx)) deltas

canMove : Point -> Point -> Bool
canMove (_, 'S') (_, v) = ord v - ord 'a' <= 1
canMove (_, c) (_, 'E') = ord 'z' - ord c <= 1
canMove (_, c) (_, v) = ord v - ord c == 1

populateNeighbors : Graph -> Graph
populateNeighbors graph = foldl neighbors graph $ keys graph
    where 
        neighbors : Graph -> Point -> Graph
        neighbors g p = 
            let
                nbrs = filter (\nbr => (fst nbr `elem` (dirs p)) && canMove p nbr) $ keys g
            in
                trace "point \{show p}"
                trace "dirs \{show (dirs p)}"
                trace "nbrs \{show nbrs}"
                trace "=================================================="
                insert p nbrs g

toGraph : List Point -> Graph
toGraph = populateNeighbors . fromList . map ((, []))


main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (rows, _) = parse parseRows symbols | Left error => printLn error
    putStr $ (unlines . map show . Data.SortedMap.toList . toGraph . coordsToPoints . mapToCoords) rows

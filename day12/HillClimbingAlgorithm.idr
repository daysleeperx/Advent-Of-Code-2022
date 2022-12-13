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

populateNeighbors : Graph -> Graph
populateNeighbors graph = foldl neighbors graph $ keys graph
    where 
        neighbors : Graph -> Point -> Graph
        neighbors g p = 
            let
                nbrs = filter (\(coord, v) => (coord `elem` (dirs p)) && ord v - ord (snd p) == 1) $ keys g
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

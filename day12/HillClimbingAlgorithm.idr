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

implementation Show (ListPair Point) where
    show (LP back front) = "\{show $ front ++ (reverse back)}"

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
canMove (_, c) (_, v) = ord v - ord c <= 1

populateNeighbors : Graph -> Graph
populateNeighbors graph = foldl neighbors graph $ keys graph
    where 
        neighbors : Graph -> Point -> Graph
        neighbors g p = 
            let
                nbrs = filter (\nbr => (fst nbr `elem` (dirs p)) && canMove p nbr) $ keys g
            in
                insert p nbrs g

toGraph : List Point -> Graph
toGraph = populateNeighbors . fromList . map ((, []))

bfs : (graph : Graph) -> (frontier : ListPair Point) -> (cameFrom : SortedMap Point (Maybe Point)) -> SortedMap Point (Maybe Point)
bfs graph (LP [] []) cameFrom = cameFrom
bfs graph frontier cameFrom = 
    let
        Just (current, rest) = pop frontier | Nothing => bfs graph empty cameFrom
        Just nbrs = lookup current graph | Nothing => bfs graph rest cameFrom
        nbrs' = filter (not . (flip elem) (keys cameFrom)) nbrs
        frontier' = foldl (flip push) rest nbrs'
        cameFrom' = insertFrom (map ((, Just current)) nbrs') cameFrom
    in
        bfs graph frontier' cameFrom'

findPath : (start : Point) -> (goal : Point) -> (cameFrom : SortedMap Point (Maybe Point)) -> Maybe (List Point)
findPath start goal cameFrom = 
    if start == goal then 
        pure [] 
    else do
        (Just start') <- lookup start cameFrom | Nothing => Nothing
        path <- findPath start' goal cameFrom 
        pure (start :: path)

findMinPath : Graph -> Maybe (List Point)
findMinPath graph = do
    start <- find (( == 'S') . snd) $ keys graph
    goal <- find (( == 'E') . snd) $ keys graph
    let cameFrom = bfs graph (push start empty) (insert start Nothing empty)
    findPath goal start cameFrom

findMinPaths : Graph -> List (List Point)
findMinPaths graph = 
    let
        starts = filter (\(_, v) => v == 'a' || v == 'S') $ keys graph
        Just goal = find (( == 'E') . snd) $ keys graph | Nothing => []
        paths = map (\s => findPath goal s (bfs graph (push s empty) (insert s Nothing empty))) starts
    in
        catMaybes paths

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (rows, _) = parse parseRows symbols | Left error => printLn error
    let Just path = (findMinPath . toGraph . coordsToPoints . mapToCoords) rows | Nothing => printLn "No Path found!"
    printLn $ length path
    let paths = (findMinPaths . toGraph . coordsToPoints . mapToCoords) rows 
    printLn $ sort (map length paths)
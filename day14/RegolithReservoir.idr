-- Day 14: Regolith Reservoir

module RegolithReservoir
import System
import System.File
import Data.String
import Data.List
import Data.String.Parser
import Data.SortedSet
import Debug.Trace


Coord : Type
Coord = (Integer, Integer)

Path : Type
Path = List Coord

InputType : Type
InputType = List Path

parseCoord : Parser Coord
parseCoord = pure (,) <*> (integer <* char ',') <*> integer

parsePath : Parser Path
parsePath = sepBy parseCoord $ string " -> "

parseInput : Parser InputType
parseInput = sepBy parsePath $ char '\n'

testSet : SortedSet Coord
testSet = empty

data Material = Rock | Sand

implementation Show Material where
    show Rock = "#"
    show Sand = "o"

Pos : Type
Pos = (Coord, Material)

implementation Eq Pos where
    (==) (coord1, z) (coord2, w) = coord1 == coord2

implementation Ord Pos where
    compare (coord1, z) (coord2, y) = coord1 `compare` coord2

Cave : Type
Cave = SortedSet Pos

implementation [CaveShowImpl] Show Cave where
    show cave = 
        let
            cave' = Data.SortedSet.toList cave
            minX = foldl min 2147483647 $ map (fst . fst) cave'
            maxX = foldl max (-2147483648) $ map (fst . fst) cave'
            maxY = foldl max (-2147483648) $ map (snd . fst) cave'

            row = zip [0..((cast maxX) `minus` (cast minX))] $ replicate (S $ (cast maxX) `minus` (cast minX)) "."
            rows = zip [0..maxY] $ replicate (S $ cast maxY) row
        in
            (unlines . map concat) (map (\(r, row) => map (\(c, v) => if (((cast c) + (cast minX), r), Rock) `contains` cave then show Rock else v) row) rows)


unfoldRange : Coord -> Coord -> List Pos
unfoldRange (x1, y1) (x2, y2) = if x1 == x2 then map ((, Rock) . (x1,)) [y1..y2] else map ((, Rock) . (, y1)) [x1..x2]

unfoldPath : Path -> Cave
unfoldPath (x :: (y :: xs)) = fromList (unfoldRange x y) `union` unfoldPath (y :: xs)
unfoldPath _ = empty

unfoldPaths : InputType -> Cave
unfoldPaths = concatMap unfoldPath

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    (putStr . show @{CaveShowImpl}) $ unfoldPaths input

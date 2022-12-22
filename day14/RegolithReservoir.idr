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

implementation Eq Material where
    (==) x y = (show x) == (show y)

Pos : Type
Pos = (Coord, Material)

implementation Ord Pos where
    (<) (coord1, _) (coord2, _) = coord1 < coord2

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
            (unlines . map concat) (map (\(r, row) => map (\(c, v) => 
                if 
                    (((cast c) + (cast minX), r), Rock) `contains` cave then 
                        "#"
                else if 
                    (((cast c) + (cast minX), r), Sand) `contains` cave then 
                        "o"
                else 
                    v
            ) row) rows)

unfoldRange : Coord -> Coord -> List Pos
unfoldRange (x1, y1) (x2, y2) = if x1 == x2 then map ((, Rock) . (x1,)) [y1..y2] else map ((, Rock) . (, y1)) [x1..x2]

unfoldPath : Path -> Cave
unfoldPath (x :: (y :: xs)) = fromList (unfoldRange x y) `union` unfoldPath (y :: xs)
unfoldPath _ = empty

unfoldPaths : InputType -> Cave
unfoldPaths = concatMap unfoldPath

maxY : Cave -> Integer
maxY = foldl max (-2147483648) . map (snd . fst) . filter ((== Rock) . snd) . Data.SortedSet.toList

data Dir = Down | Left | Right

canMove : Dir -> Coord -> Cave -> Bool
canMove Down (x, y) cave = not $ contains ((x, y + 1), Rock) cave || contains ((x, y + 1), Sand) cave
canMove Left (x, y) cave = not $ contains ((x - 1, y + 1), Rock) cave || contains ((x - 1, y + 1), Sand) cave
canMove Right (x, y) cave = not $ contains ((x + 1, y + 1), Rock) cave || contains ((x + 1, y + 1), Sand) cave

simulate : Integer -> Coord -> Cave -> Coord
simulate maxY (x, y) cave =  
    if 
        not $ y < (maxY + 1) then 
            (x, y)
    else if 
        canMove Down (x, y) cave then 
            simulate maxY (x, y + 1) cave
    else if 
        canMove Left (x, y) cave then 
            simulate maxY (x - 1, y + 1) cave
    else if 
        canMove Right (x, y) cave then 
            simulate maxY (x + 1, y + 1) cave
    else 
        (x, y)

simulate' : (maxY: Integer) -> Cave -> Cave
simulate' maxY cave =
    let
        lastPos = simulate maxY (500, 0) cave
    in
        (if lastPos /= (500, 0) then simulate' maxY (insert (lastPos, Sand) cave) else (insert (lastPos, Sand) cave))

countStopped : Cave -> Nat
countStopped cave = length . filter ((== Sand) . snd) . Data.SortedSet.toList . simulate' (maxY cave) $ cave

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let cave = unfoldPaths input
    -- (putStr . show @{CaveShowImpl}) $ simulate' (maxY cave) cave
    printLn $ countStopped cave

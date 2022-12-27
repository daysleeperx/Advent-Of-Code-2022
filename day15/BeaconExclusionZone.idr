-- Day 15: Beacon Exclusion Zone

module BeaconExclusionZone
import System
import System.File
import Data.List
import Data.String.Parser
import Debug.Trace


Coord : Type
Coord = (Integer, Integer)

record Sensor where
    constructor MkSensor
    pos, closestBeacon : Coord
    radius : Integer

implementation Show Sensor where
    show (MkSensor pos closestBeacon radius) = "pos=\{show pos}, closestBeacon=\{show closestBeacon} radius=\{show radius}"

manhattanDistance : Coord -> Coord -> Integer
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

InputType : Type
InputType = List Sensor

parseCoord : Parser Coord
parseCoord = pure (,) <*> (string "x=" *> integer) <*> (string ", y=" *> integer)

parseSensor : Parser Sensor
parseSensor = do
    skip $ string "Sensor at "
    pos <- parseCoord
    skip $ string ": closest beacon is at "
    closestBeacon <- parseCoord
    pure (MkSensor pos closestBeacon $ manhattanDistance pos closestBeacon)

parseInput : Parser InputType
parseInput = sepBy parseSensor $ char '\n'

minX : Sensor -> Integer
minX (MkSensor (x, y) _ radius) = x - radius 

maxX : Sensor -> Integer
maxX (MkSensor (x, y) _ radius) = x + radius

inRange : Sensor -> Coord -> Bool
inRange s c = manhattanDistance c s.pos <= s.radius

countBeaconExclusionZoneRow : Integer -> InputType -> Nat
countBeaconExclusionZoneRow y inp =   
    let
        minX = foldl min 2147483648 $ map minX inp
        maxX = foldl max (-2147483648) $ map maxX inp
    in
        length . filter (\x => any (\s => inRange s (x, y)) inp) $ [minX..maxX]

sensorOutsideBorder : Sensor -> List Coord
sensorOutsideBorder (MkSensor (x, y) _ radius) =
    let
        d = radius + 1
        topLeft = [(x, y) | (x, y) <- zip [x - d..x] [y..y + d], x >= 0, y >= 0, x <= 4000000, y <= 4000000]
        topRight = [(x, y) | (x, y) <- zip [x + d..x] [y..y + d], x >= 0, y >= 0, x <= 4000000, y <= 4000000]
        bottomLeft = [(x, y) | (x, y) <- zip [x - d..x] [y..y - d], x >= 0, y >= 0, x <= 4000000, y <= 4000000]
        bottomRight = [(x, y) | (x, y) <- zip [x + d..x] [y..y - d], x >= 0, y >= 0, x <= 4000000, y <= 4000000]
    in
        nub $ topLeft ++ topRight ++ bottomLeft ++ bottomRight

findBeacon : InputType -> Maybe Integer
findBeacon xs = do
        (x', y') <- head' . filter (\pos => all (not . (flip inRange) pos) xs) $ (nub . concatMap sensorOutsideBorder) xs
        pure $ x' * 4000000 + y'

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ countBeaconExclusionZoneRow 2000000 input
    printLn $ findBeacon input
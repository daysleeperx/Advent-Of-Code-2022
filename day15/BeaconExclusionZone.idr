-- Day 15: Beacon Exclusion Zone

module BeaconExclusionZone
import System
import System.File
import Data.List
import Data.List1
import Data.Nat
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

record BeaconRange (maxLimit : Nat) where
    constructor MkRange
    start, end : Nat
    valid : LTE start end
    bounded: LTE end maxLimit

implementation Show (BeaconRange maxLimit) where
    show (MkRange start end _ _) = "(start=\{show start}, end=\{show end})"

implementation Eq (BeaconRange maxLimit) where
    (==) (MkRange s1 e1 _ _) (MkRange s2 e2 _ _) = s1 == s2 && e1 == e2

implementation Ord (BeaconRange maxLimit) where
    compare (MkRange s1 _ _ _) (MkRange s2 _ _ _) = compare s1 s2

extractRange: (maxLimit : Nat) -> Integer -> Sensor -> Maybe $ BeaconRange maxLimit
extractRange maxLimit y' (MkSensor (x, y) _ radius) = do
    let d = radius - (abs (y' - y))
    let start = integerToNat $ x - d
    let end = min (integerToNat $ x + d) maxLimit
    let Yes valid = isLTE start end | No _ => Nothing
    let Yes bounded = isLTE end maxLimit | No _ => Nothing
    pure $ MkRange start end valid bounded

mergeRanges : {maxLimit : Nat} -> List (BeaconRange maxLimit) -> List $ BeaconRange maxLimit
mergeRanges xs = do
    let sorted = sort xs
    let Just (MkRange start end v b) = head' sorted | Nothing => []
    forget (foldl merge ((MkRange start end v b) ::: []) $ drop 1 sorted)
        where
            merge : List1 (BeaconRange maxLimit) -> BeaconRange maxLimit -> List1 $ BeaconRange maxLimit
            merge acc (MkRange start end valid bounded) =
                let
                    (MkRange start' end' _ _) = head acc
                    Yes overlapsStart = isLTE start (S end') | No _ => (MkRange start end valid bounded) ::: forget acc
                    end'' = max end end'
                    (Yes valid) = isLTE start' end'' | No _ => acc
                    (Yes bounded) = isLTE end'' maxLimit | No _ => acc
                in
                    (MkRange start' end'' valid bounded) ::: (tail acc)

tuningFreq : (maxLimit : Nat) -> InputType -> Maybe Integer
tuningFreq maxLimit xs = do
    let rowRanges = map (\row => ((row, ) . reverse . mergeRanges . catMaybes . map (extractRange maxLimit row)) xs) [0..cast maxLimit]
    (y, ranges) <- find ((== 2) . length . snd) rowRanges
    (MkRange _ x _ _) <- head' ranges
    (pure . cast) $ (S x) * 4000000 + (cast y)

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ tuningFreq 4000000 input

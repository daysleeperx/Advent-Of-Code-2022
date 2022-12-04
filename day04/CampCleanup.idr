-- Day 4: Camp Cleanup

module CampCleanup
import System
import System.File
import Data.Nat
import Data.List
import Data.String.Parser
import Debug.Trace


Range : Type
Range = (Nat, Nat)

SectionAssignment : Type
SectionAssignment = (Range, Range)

parseRange : Parser Range
parseRange = (,) <$> (natural <* (char '-')) <*> natural

parseSectionAssignment : Parser SectionAssignment
parseSectionAssignment = (,) <$> (parseRange <* (char ',')) <*> parseRange

parseInput : Parser $ List SectionAssignment
parseInput = sepBy parseSectionAssignment $ char '\n'

record Range' where
    constructor MkRange
    from, to : Nat
    valid : LTE from to

implementation Show Range' where
    show (MkRange from to _) = "\{show from}-\{show to}"

SectionAssignment' : Type
SectionAssignment' = (Range', Range')

InputType : Type
InputType = List SectionAssignment'

validateSection : SectionAssignment -> Maybe SectionAssignment'
validateSection ((x, z), (y, w)) = do
    let Yes prf1 = isLTE x z | No contra => Nothing
    let Yes prf2 = isLTE y w | No contra => Nothing
    Just (MkRange x z prf1, MkRange y w prf2)

validateInput : List SectionAssignment -> Maybe InputType
validateInput = traverse validateSection

contains' : Range' -> Range' -> Bool
contains' (MkRange f1 t1 _) (MkRange f2 t2 _) = (f1 >= f2 && f1 <= t2 && t1 <= t2 && t1 >= f2)

contains : Range' -> Range' -> Bool
contains x y = contains' x y || (flip contains') x y

countContaining : InputType -> Nat
countContaining = length . filter (uncurry contains)

overlaps' : Range' -> Range' -> Bool
overlaps' (MkRange f1 t1 _) (MkRange f2 t2 _) = (f1 >= f2 && f1 <= t2) || (t1 >= f2 && t1 <= t2)

overlaps : Range' -> Range' -> Bool
overlaps x y = overlaps' x y || (flip overlaps') x y

countOverlapping : InputType -> Nat
countOverlapping = length . filter (\s => (uncurry overlaps) s || (uncurry contains) s)

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let (Just input') = validateInput input | Nothing => printLn "Invalid ranges were provided!"
    printLn $ countContaining input'
    printLn $ countOverlapping input'

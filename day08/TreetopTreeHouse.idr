-- Day 8: Treetop Tree House

{-- Solution based on https://work.njae.me.uk/2022/12/08/advent-of-code-2022-day-8/ --}

module TreetopTreeHouse
import System
import System.File
import Data.List
import Data.List1
import Data.Vect
import Data.Stream
import Data.String.Parser
import Debug.Trace


{-- Types --}

Grid : Nat -> Type -> Type
Grid len a = Vect len $ Vect len a

Tree : Type
Tree = (Nat, Bool)

height : Tree -> Nat
height = fst

isVisible : Tree -> Bool
isVisible = snd

ForestRow : Type
ForestRow = (len ** Vect len Tree)

Forest : Nat -> Type
Forest len = Grid len Tree

{-- Parsing --}

InputType : Type
InputType = List1 $ ForestRow

parseTree : Parser Tree
parseTree = (, False) . finToNat <$> digit

getLength : List a -> (len ** Vect len a)
getLength [] = (Z ** [])
getLength (x :: xs) with (getLength xs)
    _ | (len ** xs') = (S len ** x :: xs')

parseForestRow : Parser ForestRow
parseForestRow = getLength <$> many parseTree

parseInput : Parser InputType
parseInput = sepBy1 parseForestRow $ char '\n'

{-- Validation --}

InputType' : Type
InputType' = (len ** Forest len)

exactLengths : (len : Nat) -> Traversable t => t (n ** Vect n a) -> Maybe (t (Vect len a))
exactLengths len xs = traverse (\(_ **  trees) => exactLength len trees) xs

validateInput : InputType -> Maybe InputType'
validateInput ((len ** row) ::: tail) = do
    rows <- exactLengths len tail 
    let (_ ** rows') = getLength (row :: rows)
    rows'' <- exactLength len rows'
    pure (len ** rows'')

{-- Solution --}

setVisible : (max : Maybe Nat) -> Vect len Tree -> Vect len Tree
setVisible _ [] = []
setVisible Nothing ((height, _) :: xs) = (height, True) :: setVisible (Just height) xs
setVisible (Just max') ((height, v) :: xs) = (height, (height > max' || v)) :: setVisible (Just $ max height max') xs

setVisibleGrid : Forest len -> Forest len
setVisibleGrid = map $ setVisible Nothing

rotateGrid : {len : Nat} -> Forest len -> Forest len
rotateGrid = map reverse . transpose

iterateN' : (n : Nat) -> (f : a -> a) -> a -> a
iterateN' Z _ = id
iterateN' (S n) f = f . iterateN' n f

setVisibleAllDirs : {len : Nat} -> (grid : Forest len) -> Forest len
setVisibleAllDirs = iterateN' 4 $ rotateGrid . setVisibleGrid

countVisible : {len : Nat} -> Forest len -> Nat
countVisible = length . filter isVisible . toList . Data.Vect.concat . setVisibleAllDirs

getCoords : (len : Nat) -> List (Nat, Nat)
getCoords Z = []
getCoords (S len) = [(row, col) | row <- [0..len], col <- [0..len]]

coordToFins : (len : Nat) -> (Nat, Nat) -> Maybe (Fin len, Fin len)
coordToFins len (row, col) = do
    row' <- natToFin row len
    col' <- natToFin col len
    pure (row', col')

coordsToFins : (len : Nat) -> (coords : List (Nat, Nat)) -> List (Fin len, Fin len)
coordsToFins len = catMaybes . map (coordToFins len)

directions : {len : Nat} -> (row : Fin len) -> (col : Fin len) -> Forest len -> List (List Tree)
directions row col forest = 
    let
        horizontal = toList $ index row forest
        vertical = toList $ index col $ transpose forest
        (left, right) = splitAt (finToNat col) horizontal
        (up, down) = splitAt (finToNat row) vertical
    in
        [reverse left, drop 1 right, reverse up, drop 1 down]

takeWhile' : (a -> Bool) -> List a -> List a
takeWhile' f = unfoldr take' 
   where
       take' :  List a -> Maybe (a, List a)
       take' [] = Nothing
       take' (x :: xs) = if (f x) then Just (x, xs) else Just (x, [])

getDistance : (h: Nat) -> List Tree -> Nat
getDistance h = length . takeWhile' ( < h) . map height

scenicScore : {len : Nat} -> Forest len -> (row : Fin len) -> (col : Fin len) -> Nat
scenicScore forest row col = 
    let
        dirs = directions row col forest
        h = height (index col $ index row forest) 
    in
        foldl (*) 1 $ map (getDistance h) dirs

getMaxScenicScore : {len : Nat} -> Forest len -> Nat
getMaxScenicScore forest = foldl max Z $ map (uncurry (scenicScore forest)) $ coordsToFins len (getCoords len)

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let Just (_ ** forest) = validateInput input | Nothing => printLn "Forest is not well formed!"
    printLn $ countVisible forest
    printLn $ getMaxScenicScore forest

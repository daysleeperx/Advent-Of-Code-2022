-- Day 8: Treetop Tree House

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

isVisible : Tree -> Bool
isVisible (_, visible) = visible

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
setVisibleAllDirs = iterateN' 4 (rotateGrid . setVisibleGrid)

countVisible : {len : Nat} -> Forest len -> Nat
countVisible = length . filter isVisible . toList . Data.Vect.concat . setVisibleAllDirs

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let Just (_ ** grid) = validateInput input | Nothing => printLn "Grid is not well formed!"
    printLn $ countVisible grid

-- Day 5: Supply Stacks

module SupplyStacks
import System
import System.File
import Data.List
import Data.List1
import Data.List.Alternating
import Data.Vect
import Data.String.Parser
import Debug.Trace


{-- Types --}

interface Stack (stack: Type -> Type) where
    empty: stack a
    push: a -> stack a -> stack a
    pop: stack a -> Maybe (a, stack a)

implementation Stack List where
    empty = []
    push x xs = (x :: xs)
    pop [] = Nothing
    pop (x :: xs) = Just (x, xs)

{-- Parsing --}

data Procedure = Move Nat Nat Nat

Crate : Type
Crate = Maybe Char

CargoRow : Type
CargoRow = (len ** Vect len Crate)

Cargo : Type
Cargo = List1 CargoRow

InputType : Type
InputType = (Cargo, List Procedure)

parseCrate : Parser Crate
parseCrate = Just <$> (char '[' *> letter <* char ']') 
    <|> ntimes 3 space $> Nothing

getLength : List a -> (len ** Vect len a)
getLength [] = (Z ** [])
getLength (x :: xs) with (getLength xs)
    _ | (len ** xs') = (S len ** x :: xs')

parseRow : Parser CargoRow
parseRow = getLength <$> many parseCrate

parseCargo : Parser Cargo
parseCargo = sepBy1 parseRow $ char '\n'

parseProcedure : Parser Procedure
parseProcedure = Move <$> part <*> part <*> part
    where
        part : Parser Nat
        part = lexeme (token "move" 
            <|> token " from" 
            <|> token " to") *> natural

parseProcedures : Parser $ List Procedure
parseProcedures = sepBy parseProcedure $ char '\n'

parseInput : Parser InputType
parseInput = do
    cargo <- parseCargo
    _ <- skip (alternating spaces1 natural)
    _ <- skip spaces
    procedures <- parseProcedures
    pure (cargo, procedures)

{-- Validation --}

record Procedure' (length : Nat) where
    constructor MkProcedure
    items : Nat
    from, to : Fin length

implementation Show (Procedure' n) where
    show (MkProcedure items from to) = "Move \{show items} from \{show from} to \{show to}"

record InputType' where
    constructor MkInputType
    length : Nat
    stacks : Vect length $ List Char
    procedures: List $ Procedure' (S length) -- FIXME: the (S length) could be removed if inputs would be normalized

implementation Show InputType' where
    show (MkInputType length stacks procedures) = "len: \{show length} stacks: \{show stacks} procedures: \{show procedures}"

exactLengths : (len : Nat) -> Traversable t => t (n ** Vect n a) -> Maybe (t (Vect len a))
exactLengths len xs = traverse (\(_ **  bits) => exactLength len bits) xs

validateCargo : List1 CargoRow -> Maybe (len ** List1 (Vect len Crate))
validateCargo ((n ** row) ::: tail) = exactLengths n tail >>= (\rows => pure (n ** row ::: rows))

validateProcedure : (n : Nat) -> Procedure -> Maybe $ Procedure' n
validateProcedure n (Move c from to) = do
    from' <- natToFin from n
    to' <- natToFin to n
    pure $ MkProcedure c from' to'

validateProcedures : (n : Nat) -> List Procedure -> Maybe $ List $ Procedure' n
validateProcedures n = traverse $ validateProcedure n

vectToList : Vect n a -> List a
vectToList xs = foldl snoc [] xs

validateInput : InputType -> Maybe InputType'
validateInput (cargo, procedures) = do
    normalized <- fromList $ init cargo -- FIXME: parser returns empty list as last element
    (n ** cargo') <- validateCargo normalized
    procedures <- validateProcedures (S n) procedures
    let stacks = (map (catMaybes . vectToList) . transpose) (Data.Vect.fromList $ forget cargo')
    pure $ MkInputType n stacks procedures

{-- Solution --}

rearrange : InputType' -> Maybe InputType'
rearrange (MkInputType length stacks []) = Just (MkInputType length stacks [])
rearrange (MkInputType length stacks ((MkProcedure items from to) :: xs)) =
    let
        FS from' = from | FZ => Nothing
        FS to' = to | FZ => Nothing
        (popped, fromStack') = splitAt items $ index from' stacks
        stacks' =
            (updateAt from' (const fromStack') . updateAt to' (\toStack => {-- replace with foldl (flip push) for Part 1 --} foldr push toStack popped)) stacks
    in
        rearrange (MkInputType length stacks' xs)

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let Just input' = validateInput input | Nothing => printLn "Incorrent cargo dimensions!"
    let Just res = rearrange input' | Nothing => printLn "An error occured while executing procedures!"
    printLn $ (pack . concat . map (take 1)) res.stacks

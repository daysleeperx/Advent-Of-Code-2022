-- Day 11: Monkey In The Middle

module MonkeyInTheMiddle
import System
import System.File
import Data.List
import Data.Vect
import Data.String.Parser
import Debug.Trace

{-- Great explanation of Part 2 :
    https://www.reddit.com/r/adventofcode/comments/zifqmh/comment/izv7hpx/?utm_source=share&utm_medium=web2x&context=3 
--}


WorryLevel : Type
WorryLevel = Integer

record Monkey where
    constructor MkMonkey
    levels : List WorryLevel
    operation : WorryLevel -> WorryLevel
    test : WorryLevel -> Nat
    activity : Nat

implementation Show Monkey where
    show (MkMonkey levels operation test activity) = "Monkey (worryLevels=\{show levels}, activity=\{show activity})"

{-- Parsing --}

parseWorryLevel : Parser WorryLevel
parseWorryLevel = space *> integer

parseWorryLevels : Parser $ List WorryLevel
parseWorryLevels = string "Starting items:" *> commaSep parseWorryLevel

parseOperator : Parser (WorryLevel -> WorryLevel -> WorryLevel)
parseOperator = lexeme (char '+' $> (+) <|> char '*' $> (*))

data Operand = Old | Const Integer

parseOperand : Parser Operand
parseOperand = Const <$> lexeme integer <|> token "old" $> Old

extractOperand : Operand -> Integer -> Integer
extractOperand (Const x) _ = x
extractOperand _ y = y

parseOperation : Parser (WorryLevel -> WorryLevel)
parseOperation = do
    skip $ string "Operation: new = "
    x <- parseOperand
    op <- parseOperator
    y <- parseOperand
    pure (\old => extractOperand x old `op` extractOperand y old)

parseTest : Parser (WorryLevel -> Nat)
parseTest = do
    skip $ string "Test: divisible by "
    m <- integer
    skip $ char '\n' *> spaces
    t <- string "If true: throw to monkey " *> natural
    skip $ char '\n' *> spaces
    f <- string "If false: throw to monkey " *> natural
    pure (\worry => if worry `mod` m == 0 then t else f)

parseMonkey : Parser Monkey
parseMonkey = do
    skip $ takeWhile (/= '\n') *> char '\n' *> spaces
    levels <- parseWorryLevels
    skip $ char '\n' *> spaces
    op <- parseOperation
    test <- parseTest
    pure (MkMonkey levels op test Z)

getLength : List a -> (len ** Vect len a)
getLength [] = (Z ** [])
getLength (x :: xs) with (getLength xs)
    _ | (len ** xs') = (S len ** x :: xs')

parseMonkeys : Parser (len ** Vect len Monkey)
parseMonkeys = getLength <$> (sepBy parseMonkey spaces)

InputType : Type
InputType = (len ** Vect len Monkey)

parseInput : Parser InputType
parseInput = parseMonkeys

{-- Solution --}

inspect : {len : Nat} -> Vect len Monkey -> Fin len -> Vect len Monkey
inspect monkeys from = let monkey = index from monkeys in 
    case monkey.levels of
        [] => monkeys
        (item :: xs) =>
            let
                worry = (cast (monkey.operation item)) / 3.0 -- TODO: extract to func for part 2
                to = monkey.test (cast worry)
                Just to' = natToFin to len | Nothing => monkeys
                monkey' = { levels := xs, activity $= S } monkey
                monkeys' = (updateAt from (const monkey') . updateAt to' (\m => { levels := snoc m.levels (cast worry) } m)) monkeys
            in
                inspect monkeys' from

round : (inp : InputType) -> List (Fin (fst inp)) -> InputType
round (len ** monkeys) xs = (len ** foldl inspect monkeys xs)

iterateN' : (n : Nat) -> (f : a -> a) -> a -> a
iterateN' Z _ = id
iterateN' (S n) f = f . iterateN' n f

nRounds : Nat -> (inp : InputType) -> List (Fin (fst inp)) -> InputType
nRounds n inp xs = iterateN' n (\inp' => round inp' $ findIndices (\_ => True) (snd inp')) inp

sumMosttActive : InputType -> Nat
sumMosttActive (_ ** monkeys) = (product . take 2 . sortBy (flip compare) . map activity . toList) monkeys
    
main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    let result = nRounds 20 input $ findIndices (\_ => True) (snd input)
    printLn $ sumMosttActive result

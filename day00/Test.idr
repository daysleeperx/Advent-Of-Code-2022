-- Day 0: Test

module Test
import System
import System.File
import Data.List
import Data.String.Parser
import Debug.Trace

data Tree : Type -> Type where
    Leaf : Tree a
    Node : (left : Tree a) -> (this : a) -> (right : Tree a) -> Tree a

implementation Functor Tree where
    map f Leaf = Leaf
    map f (Node left this right) =
        let
            left' = map f left
            right' = map f right
        in
            Node left' (f this) right'

t1 : Tree Nat
t1 = Node (Node Leaf 2 Leaf) 10 Leaf

t2 : Tree Nat
t2 = map ( + 2) t1

t3 : Tree Nat
t3 = map S t2

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    pure ()

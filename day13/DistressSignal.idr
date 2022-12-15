-- Day 13: Distress Signal

module DistressSignal
import System
import System.File
import Data.List
import Data.String.Parser
import Debug.Trace


data Packet = Val Integer | Lst (List Packet)

partial
implementation Show Packet where
    show (Val x) = show x 
    show (Lst xs) = show xs

partial
implementation Eq Packet where
    (==) a b = show a == show b

partial
implementation Ord Packet where
    compare (Val x) (Val y) = compare x y
    compare (Val x) (Lst right) = compare (Lst [Val x]) (Lst right)
    compare (Lst left) (Val y) = compare (Lst left) (Lst [Val y])
    compare (Lst []) (Lst []) = EQ
    compare (Lst []) (Lst (_ :: _)) = LT
    compare (Lst (_ :: _)) (Lst []) = GT
    compare (Lst (x :: xs)) (Lst (y :: ys)) with (compare x y == EQ)
      _ | True = compare (Lst xs) (Lst ys)
      _ | _ = compare x y

PacketPair : Type
PacketPair = (Packet, Packet)

parsePacket : Parser Packet
parsePacket = Val <$> integer 
            <|>  char '[' *> Lst <$> (commaSep parsePacket) <* char ']'

parsePacketPair : Parser PacketPair
parsePacketPair = pure (,) <*> (parsePacket <* char '\n') <*> parsePacket

InputType : Type
InputType = List PacketPair

parseInput : Parser InputType
parseInput = sepBy parsePacketPair spaces

sumCorrectOrderIdx : (pairs : InputType) -> Nat
sumCorrectOrderIdx pairs = (sum . map fst . filter snd . zip [1..length pairs] . map (uncurry (<))) pairs

decoderKey : (pairs: InputType) -> Nat
decoderKey pairs =
    let
        dividers = [Lst [Lst [Val 2]], Lst [Lst [Val 6]]]
        pairs' = dividers ++ (concatMap (\(a, b) => [a, b]) pairs) 
    in
        (product . map fst . filter ((flip elem) dividers . snd) . zip [1..length pairs'] . sort) pairs'

main : IO ()
main = do
    args <- getArgs
    let (file :: _) = drop 1 args | [] => printLn "No file provided!"
    (Right symbols) <- readFile file | (Left error) => printLn error
    let Right (input, _) = parse parseInput symbols | Left error => printLn error
    printLn $ sumCorrectOrderIdx input
    printLn $ decoderKey input
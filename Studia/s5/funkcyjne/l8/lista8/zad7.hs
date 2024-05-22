import Control.Monad
import Data.Char
import STM
import System.IO

data BF
    = MoveR -- >
    | MoveL -- <
    | Inc -- +
    | Dec -- -
    | Output -- .
    | Input -- ,
    | While [BF] -- [ ]

instance Show BF where
    show MoveR = "MoveR"
    show MoveL = "MoveL"
    show Inc = "Inc"
    show Dec = "Dec"
    show Output = "Output"
    show Input = "Input"
    show (While bfList) = "While: " ++ show bfList

brainfuckParser :: StreamTrans Char BF ()
brainfuckParser =
    _brainfuckParser [] []
  where
    _brainfuckParser acc stack =
        ReadS
            ( \input -> case input of
                Nothing -> Return ()
                Just x ->
                    let bfConstr = case x of
                            '>' -> Just MoveR
                            '<' -> Just MoveL
                            '+' -> Just Inc
                            '-' -> Just Dec
                            '.' -> Just Output
                            ',' -> Just Input
                            _ -> Nothing
                     in case (x, bfConstr) of
                            ('[', _) -> _brainfuckParser [] (acc : stack)
                            (']', _) -> case stack of
                                [] -> WriteS (While (reverse acc)) brainfuckParser
                                [[]] -> WriteS (While (reverse acc)) brainfuckParser
                                x : xs -> _brainfuckParser ((While (reverse acc)) : x) xs
                            (_, Just x) -> case stack of
                                [] -> WriteS x brainfuckParser
                                _ -> _brainfuckParser (x : acc) stack
                            (_, Nothing) -> _brainfuckParser acc stack
            )

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

type Tape = ([Int], Int, [Int])
evalBF :: Tape -> BF -> StreamTrans Char Char Tape
evalBF (l, p, h : r) MoveR = Return (p : l, h, r)
evalBF (h : l, p, r) MoveL = Return (l, h, p : r)
evalBF (l, p, r) Inc = Return (l, p + 1, r)
evalBF (l, p, r) Dec = Return (l, p - 1, r)
evalBF (l, p, r) Output = WriteS (coerceEnum p) (Return (l, p, r))
evalBF (l, p, r) Input =
    ReadS
        ( \inp -> case inp of
            Just x -> Return (l, (coerceEnum x), r)
            Nothing -> error "error"
        )
evalBF (l, p, r) (While bfList) =
    if p == 0
        then Return (l, p, r)
        else do
            t <- evalBFBlock (l, p, r) bfList
            evalBF t (While bfList)

evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape
evalBFBlock = foldM evalBF

runBF :: [BF] -> StreamTrans Char Char ()
runBF bfList = do
    x <- evalBFBlock (repeat 0, 0, repeat 0) bfList
    Return ()


main :: IO ()
main = do
    hdl <- openFile "program.bf" ReadMode
    contents <- hGetContents hdl
    runIOStreamTrans (runBF (fst (listTrans brainfuckParser contents)))

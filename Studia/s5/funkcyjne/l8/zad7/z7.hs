import System.IO 
import Data.Char 
import Control.Monad

data StreamTrans i o a
    = Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)


toLowerStream :: StreamTrans Char Char a
toLowerStream = ReadS $ \x ->
    case x of 
        Nothing -> toLowerStream
        Just x -> WriteS (toLower x) (toLowerStream)


runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans (Return a) = return a
runIOStreamTrans (ReadS f) = do
    check <- isEOF
    if check 
        then runIOStreamTrans (f Nothing)
        else do
            input <- getChar
            runIOStreamTrans $ f (Just input)
runIOStreamTrans (WriteS o s) = do
    putChar o
    runIOStreamTrans s 

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) _ = ([], a)
listTrans (ReadS f) [] = listTrans (f Nothing) []
listTrans (ReadS f) (x : xs) = listTrans (f (Just x)) xs
listTrans (WriteS o s) xs = (o : os, a)
  where
    (os, a) = listTrans s xs

(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
(|>|) _ (Return b) = (Return b)
(|>|) (Return a) (ReadS k) = (Return a) |>| (k Nothing)
(|>|) (ReadS c) (ReadS k) = ReadS (\ input -> (c input) |>| (ReadS k))
(|>|) (WriteS o c) (ReadS k) = c |>| (k (Just o))
(|>|) st (WriteS o k) = WriteS o (st |>| k)

-- Zadanie 7

data BF
    = MoveR -- >
    | MoveL -- <
    | Inc -- +
    | Dec -- -
    | Output -- .
    | Input -- ,
    | While [BF] -- [ ]

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
                                [[]] -> WriteS (While (reverse acc)) brainfuckParser
                                x : xs -> _brainfuckParser ((While (reverse acc)) : x) xs
                            (_, Just x) -> case stack of
                                [] -> WriteS x brainfuckParser
                                _ -> _brainfuckParser (x : acc) stack
                            (_, Nothing) -> _brainfuckParser acc stack
            )

-- Zadanie 8
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

-- Zadanie 9

instance Functor (StreamTrans i o) where
  fmap f m = m >>= return . f

instance Applicative (StreamTrans i o) where
  pure = Return
  (<*>) = ap

instance Monad (StreamTrans i o) where
    return = pure
    (Return a) >>= f = f a
    (ReadS cps) >>= f = ReadS (\ inp -> cps inp >>= f)
    (WriteS o cps) >>= f = WriteS o (cps >>= f)

main :: IO ()
main = do
    hdl <- openFile "program.bf" ReadMode
    contents <- hGetContents hdl
    runIOStreamTrans (runBF (fst (listTrans brainfuckParser contents)))
import System.IO (isEOF)
import Data.Char (toLower)

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

-- Zadanie 5
(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b

(|>|) trans1 trans2 = case trans1 of
    Return result -> WriteS result trans2
    ReadS f -> ReadS $ \input -> f input |>| trans2
    WriteS o s -> WriteS o (s |>| trans2)

main :: IO ()
main = do
    runIOStreamTrans (toLowerStream |>| toLowerStream)

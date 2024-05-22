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

main :: IO ()
main = runIOStreamTrans toLowerStream 
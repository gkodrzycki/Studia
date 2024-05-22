import System.IO (isEOF)
import Data.Char (toLower)

echoLower :: IO ()
echoLower = do
    check <- isEOF
    if check 
        then return ()
        else do
            input <- getLine
            putStrLn $ map toLower input
            echoLower   

main :: IO ()
main = echoLower

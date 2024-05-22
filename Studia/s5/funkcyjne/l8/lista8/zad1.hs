import Data.Char
import System.IO (isEOF)

echoLower :: IO ()
echoLower = do
            x <- isEOF
            if x then return ()
            else do getChar>>= \ ch -> putChar (toLower ch)
                    echoLower

main :: IO ()
main = echoLower 

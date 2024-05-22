import qualified PF as PF

foo :: IO ()
foo =
  putStrLn "A"

main :: IO ()
main = do
  x <- getLine
  putStrLn x
  putStrLn x


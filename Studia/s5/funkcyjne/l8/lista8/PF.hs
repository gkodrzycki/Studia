module PF where
import Control.Monad

-- type IntList = [Integer]

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

newtype IntList = IntList { unIntList :: [Integer] }

data Vec2D = Vec2D
  { x :: Integer
  , y :: Integer
  }
  deriving Show

newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f m = m >>= return . f

instance Applicative Id where
  pure = return
  (<*>) = ap

instance Monad Id where
  return = Id
  Id x >>= f = f x

sort :: Ord a => [a] -> [a]
sort []     = []
sort [x]    = [x]
sort (x:xs) = sort (filter (<x) xs) ++ [x] ++ sort [y | y <- xs, y >= x ]

primes :: [Integer]
primes =
  2:filter isPrime [3..] where
    isPrime x = all (\ p -> x `mod` p /= 0) $ takeWhile (\p -> p*p <= x) primes



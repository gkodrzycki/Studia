{-# LANGUAGE KindSignatures #-}
module PF where

import Data.Void
import Data.Maybe
import Control.Monad

-- Fix ma rodzaj (* -> *) -> *
newtype Fix (f :: * -> *) = Fix (f (Fix f))

newtype ListF a l = ListF (Maybe (a, l))
type List a = Fix (ListF a)

class Finite a where
  allValues :: [a]

instance Finite () where
  allValues = [()]

instance Finite Bool where
  allValues = [True, False]

instance Finite Void where
  allValues = []

instance Finite a => Finite (Maybe a) where
  allValues = Nothing:map Just allValues

instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
  allValues = map toFunction allFunctions where
--    toFunction f x = fromJust $ lookup x f
--    toFunction f = fromJust . flip lookup f
    toFunction = (.) fromJust . flip lookup
    allFunctions = mapM (\ a -> [ (a,b) | b <- allValues ]) allValues

instance (Finite a, Eq b) => Eq (a -> b) where
  f == g = all (\ x -> f x == g x) allValues

instance (Finite a, Show a, Show b) => Show (a -> b) where
  show f = "|" ++ concatMap (\ x -> show x ++ " => " ++ show (f x) ++ "|") allValues

-------------------------------------------------------------------------------

class Monad m => RLMonad (m :: * -> *) where
  readLine  :: m String
  writeLine :: String -> m ()

echo :: RLMonad m => m ()
echo = do
  line <- readLine
  writeLine line
  echo

instance RLMonad IO where
  readLine  = getLine
  writeLine = putStrLn

-- Inna instancja dla RLMonad : monada stanu + writer.
-- Funkcja przyjmuje jako parametr listę wszystkich napisów które zostaną po
-- koleji przeczytane przez readLine, i zwraca trójkę: listę nieprzeczytanych
-- napisów, listę wypisanych napisów prze writeLine i wynik.
newtype RL a = RL { unRL :: [String] -> ([String], [String], a) }

run :: RL a -> [String] -> ([String], [String], a)
run (RL f) xs = f xs

-- Instancja funktora: korzysta z tego, że RL jest monadą. Instancja monady
-- zdefiniowana niżej -- w Haskellu wszystkie definicje w obrębie jednego
-- modułu są wzajemnie rekurencyjne.
instance Functor RL where
  fmap f m = do { x <- m; return $ f x }

-- Instancja funktora aplikatywnego: również wykorzystuje fakt, że RL jest
-- monadą.
instance Applicative RL where
  pure x = RL $ \ xs -> (xs, [], x)
  (<*>) = ap

instance Monad RL where
  RL f >>= g = RL $ \ xs ->
    let (ys, s1, a) = f xs in
    let (zs, s2, b) = (unRL $ g a) ys in
    (zs, s1 ++ s2, b)

instance RLMonad RL where
  readLine = RL $ \ xs -> (tail xs, [], head xs)
  writeLine s = RL $ \ xs -> (xs, [s], ())

-------------------------------------------------------------------------------

select :: MonadPlus m => Integer -> Integer -> m Integer
select a b =
  foldr (\ x m -> return x `mplus` m) mzero [a..b]

type Triple = (Integer, Integer, Integer)

triples :: MonadPlus m => Integer -> m Triple
triples n = do
  a <- select 1 n
  b <- select a n
  c <- select b n
  if a*a + b*b == c*c then return (a,b,c)
  else mzero


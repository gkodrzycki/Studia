-- Ten moduł pokazuje jak można samemu zainstalować jakiś typ w klasie Monad.
-- Dla prostoty przykładu zrobiono to dla monady identycznościowej.
module Id where

import Control.Monad

-- Dyrektywa "deriving Show" powoduje, że generowana jest instancja dla klasy
-- Show, więc interpreter potrafi wyświetlać wartości tego typu
newtype Id a = Id { unId :: a } deriving Show

-- W Haskellu żeby typ był monadą, to musi być funtkorem (Functor) i funktorem
-- aplikatywnym (Applicative). Jeśli potrafimy napisać funkcje return i (>>=),
-- to implementacja odpowiednich metod z klas Functor i Applicative może taka
-- jak poniżej (niezależnie od monady). Funkcja ap jest zdefiniowana w module
-- Control.Monad jako:
--
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
--
-- co można zapisać bezpunktowo (w tym wypadku *pointless*, a nie *pointfree*)
--
-- ap = flip (.) (flip (.) (return .) . (>>=)) . (>>=)
instance Functor Id where
  fmap f m = m >>= return . f

instance Applicative Id where
  pure = return
  (<*>) = ap

-- Natomiast implementacje metod return i (>>=) trzeba napisać dla każdej
-- monady osobno.
instance Monad Id where
  return = Id
  Id x >>= f = f x


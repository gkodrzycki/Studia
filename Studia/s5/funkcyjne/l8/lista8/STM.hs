module STM where

import Data.Char qualified as DChar
import System.IO (isEOF)
import Control.Monad

data StreamTrans i o a
    = Return a
    | ReadS (Maybe i -> StreamTrans i o a)
    | WriteS o (StreamTrans i o a)

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


toLower :: StreamTrans Char Char ()
toLower = ReadS (\ input -> 
    case input of
    Nothing -> Return ()
    Just ch -> WriteS (DChar.toLower ch) toLower)

listTrans :: StreamTrans i o a -> [i] -> ([o],a)
listTrans stream lst =
    _listTrans stream lst []
    where _listTrans st xs acc = case st of {
            Return a  -> ((reverse acc),a);
            ReadS cps -> case xs of {
                []   -> _listTrans (cps Nothing) xs acc;
                h:tl -> _listTrans (cps (Just h)) tl acc};
            WriteS o cps -> _listTrans cps xs (o : acc)}

(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
(|>|) _ (Return b) = (Return b)
(|>|) (Return a) (ReadS k) = (Return a) |>| (k Nothing)
(|>|) (ReadS c) (ReadS k) = ReadS (\ input -> (c input) |>| (ReadS k))
(|>|) (WriteS o c) (ReadS k) = c |>| (k (Just o))
(|>|) st (WriteS o k) = WriteS o (st |>| k)

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput stream =
    _captureOutput stream []
    where _captureOutput st acc = case st of {
        Return a  -> Return (a,(reverse acc));
        ReadS cps -> ReadS (\ input -> _captureOutput (cps input) acc);
        WriteS o cps -> _captureOutput cps (o : acc)}


runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans stream =
   case stream of
    Return a -> return a
    ReadS cps -> do
                x <- isEOF
                if x then runIOStreamTrans (cps Nothing)
                else getChar >>= \ ch -> runIOStreamTrans (cps (Just ch))
    WriteS o cps -> do
                    putChar o
                    runIOStreamTrans cps


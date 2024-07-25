datatype Digit = One | Two
type Nat = Digit list

fun inc [] = [One]
  | inc (One :: rest) = Two :: rest
  | inc (Two :: rest) = One :: inc rest

fun dec [] = raise Empty
  | dec [One] = []
  | dec (Two :: rest) = One :: rest
  | dec (One :: rest) = Two :: dec rest

fun add [] ys = ys
    | add xs [] = xs
    | add (One :: xs) (One :: ys) = 
        Two :: add xs ys
    | add (Two :: xs) (One :: ys) =
        One :: add (inc xs) ys
    | add (One :: xs) (Two :: ys) =
        One :: add xs (inc ys)
    | add (Two :: xs) (Two :: ys) = 
        Two :: add (inc xs) ys
        
(*
[] -> 0
[1] -> 1
[2] -> 2
[1 :: 1] -> 3  => [2]
[2 :: 1] -> 4  => [1 :: 1]
[1 :: 2] -> 5  => [2 :: 1]
[2 :: 2] -> 6  => [1 :: 2]
[1 :: 1 :: 1] -> 7 => [2 :: 2]
[2 :: 1 :: 1] -> 8
*)

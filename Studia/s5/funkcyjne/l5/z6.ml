(* Zadanie 6 *)
type 'a my_lazy =
  | Lazy  of (unit -> 'a)
  | InProgress
  | Done of 'a

let fix f =
  let x = ref InProgress in
    x := Lazy (fun () -> f x);
    x

let force x =
  match !x with
  | Done a -> a
  | InProgress -> failwith "oblicza sie"
  | Lazy f ->
      x := InProgress;
      let a = f () in
      x := Done a;
      a
    
(* Zadanie 7 *)
type 'a my_list =
    | Nil
    | Cons of 'a * 'a my_list my_lazy ref

let head (list : 'a my_list) = 
  match list with 
  | Nil -> failwith "No elements in list"
  | Cons (x, xs) -> x

let tail list = 
  match list with
  | Nil -> failwith "No elements in list"
  | Cons (x, xs) -> force xs

(* Wykład *)

let nat_from n =
  let rec succ x =
    fun nat_stream -> Cons (x, fix (succ (x+1))) in
    force (fix (succ n))

let rec for_all p xs =
  match xs with
  | Nil -> true
  | Cons(x, _) -> p x && for_all p (tail xs)

let primes =
  let is_prime n source =
    source
    |> for_all (fun p -> n mod p <> 0) in
  let rec worker n curr =
    (fun prime_stream -> 
      if is_prime n curr
        then Cons (n, fix (worker (n+1) (Cons (n, ref (Done curr)))))
        else force (fix (worker (n+1) curr))
    )
      in
  force (fix (worker 2 Nil))

let rec nth n xs =
  match xs with
  | Nil -> failwith "No elements in list"
  | Cons(x, xs) ->
    if n = 0 then x
    else nth (n-1) (force xs)

let first_n n stream =
  let rec loop i acc =
    if i >= 0 then
      let result = nth i stream in
      loop (i - 1) (result :: acc)
    else
      acc
  in
  loop n [];;

(* Testy  *)
fix (fun l -> force l);;

force (fix (fun l -> force l));;

let stream_of_ones = fix (fun stream_of_ones -> Cons(1, stream_of_ones));;
let test = nat_from 1;;
head test;;
head (tail test);;

first_n 5 primes;;

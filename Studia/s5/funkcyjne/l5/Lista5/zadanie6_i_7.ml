type 'a my_lazy = 'a sloth ref
and 'a sloth =
  | Postponement of {calculation : unit -> 'a}
  | Busy
  | Result of {return : 'a};;


let force (sinner : 'a my_lazy) : 'a = 
  match !sinner with 
  | Result {return} -> return
  | Busy -> failwith "I am working now"
  | Postponement {calculation} -> begin
     sinner := Busy; 
     let res = calculation () in sinner := Result {return = res}; res
  end;;

let fix (func : ('a my_lazy -> 'a)) : 'a my_lazy = 
  let (value : 'a my_lazy) = ref Busy in 
  value := Postponement {calculation = fun () -> (func value)}; value;;

type 'a lazy_list = 
  | Nil
  | Cons of 'a * 'a lazy_list my_lazy

let head (lista : 'a lazy_list) =
  match lista with
  | Nil -> failwith "End of List"
  | Cons(value,_) -> value;;

let tail (lista : 'a lazy_list) = 
  match lista with 
  | Nil -> failwith "End of List"
  | Cons(_,rest) -> force rest;;

let stream_of_ones = fix (fun stream_of_ones -> Cons(1, stream_of_ones));;

let nat_num_from (skip : int) : 'a lazy_list =
  let rec succ value = 
    fun nth_stream -> Cons (value, fix (succ (value+1))) in force (fix (succ skip));;

let filter (pattern: 'a -> bool) (stream : 'a lazy_list) : 'a lazy_list = 
  let rec _filter (_stream : 'a lazy_list) = 
    (fun lista -> 
      match _stream with 
      | Nil -> Nil 
      | Cons(value,rest) -> 
      if pattern value then Cons(value, fix (_filter (tail _stream))) 
      else force (fix (_filter (tail _stream)))) 
    in force (fix (_filter stream));;

let take_while (pattern : 'a -> bool) (stream : 'a lazy_list) = 
  let rec take (source : 'a lazy_list) = 
    (fun take_stream -> 
      match source with 
      | Nil -> Nil
      | Cons(value,rest) -> if pattern value
        then Cons(value,fix (take (tail source))) else Nil)
    in force (fix (take stream));;

let rec for_all pattern stream = 
  match stream with 
  | Nil -> true 
  | Cons (value,_) -> pattern value && for_all pattern (tail stream);;

let singleton value = Cons(value,ref (Ready Nil));;

let primes =
  let is_prime n source =
    source
    |> for_all (fun p -> n mod p <> 0) in
  let rec worker n curr =
    (fun prime_stream -> 
      if is_prime n curr
        then Cons (n, fix (worker (n+1) (Cons (n, ref (Ready curr)))))
        else force (fix (worker (n+1) curr))
    )
      in
  force (fix (worker 2 Nil))
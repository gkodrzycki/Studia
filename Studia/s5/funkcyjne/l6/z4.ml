let rec fold_left_cps (f : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list)  (cont : 'a -> 'c) =
  match xs with 
  | [] -> cont acc
  | x :: xs -> f acc x (fun newacc -> fold_left_cps f newacc xs cont)


let fold_left f acc xs = 
  fold_left_cps (fun acc x cont -> cont (f acc x)) acc xs (fun x -> x) 


(* Testy *)
let sum xs = fold_left_cps (fun a b cont -> cont (a+b)) 0 xs (fun x -> x)  

let test = sum [1;2;3]

let test2 = fold_left (+) 0 [1;2;3]
(* ogon *)
let rec foldl func acc xs = 
  match xs with 
  | [] -> acc
  | y :: ys -> foldl func (func y acc) ys
;;
(* nie ogon *)
let rec foldr func acc xs = 
  match xs with 
  | [] -> acc
  | y :: ys -> func y (foldr func acc) ys
;;
(* ogon *)
let rec foldr2 func acc xs = 
  let rec rev xs acc = 
    match xs with
    | [] -> acc
    | y :: ys -> rev ys (y :: acc)
  in foldl func acc (rev xs [])
;;
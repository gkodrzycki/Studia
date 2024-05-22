let rec merge cmp a b = 
  match a, b with
  | [], [] -> []
  | x :: a, [] -> x :: a
  | [], x :: b -> x :: b
  | x :: a, y :: b -> if cmp x y
    then x :: merge cmp a (y :: b)
    else y :: merge cmp (x :: a) b
;;

let rec halve xs =
  match xs with
  | [] -> [], []
  | x :: [] -> [x], []
  | x :: y :: ys -> (fun (l, r) -> (x :: l, y :: r)) (halve ys)
;;

let rec merge_sort cmp xs = 
  match (halve xs) with
  | [], [] -> []
  | [x], [] -> [x]
  | x, y -> merge cmp (merge_sort cmp x) (merge_sort cmp y)
;;

let less a b = a < b;;
let more a b = a > b;;

let test = [5;2;6;10;11;1;3;4];;

let sortedtest1 = merge_sort less test;;
let sortedtest2  = merge_sort more test;;
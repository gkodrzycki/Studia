let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h::t -> (x::lst) :: (List.map (fun el -> h::el) (insert x t))


let rec perm_insert lst =
  match lst with
  | [] -> [[]]
  | h::t -> List.flatten (List.map (insert h) (perm_insert t))
  

let rm x l = List.filter ((<>) x) l  
  
let rec perm_select list = match list with 
| [] -> []
| x::[] -> [[x]]
| l -> List.fold_left (fun acc x -> acc @ List.map (fun p -> x::p) (perm_select (rm x l))) [] l
  
let test_in = perm_insert [1;2;3]
let test_se = perm_select [1;2;3];;

(*

[] [1;2;3]

*)
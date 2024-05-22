let rec sublists list = match list with
| [] -> [[]]
| x::list -> (fun y -> List.fold_left (fun a b -> ((x::b)::a)) y y) (sublists list) 

let test_sub = sublists [1;2;3]

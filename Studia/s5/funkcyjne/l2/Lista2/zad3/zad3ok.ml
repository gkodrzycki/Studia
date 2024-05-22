let rec sufixes list = match list with
| [] -> [[]]
| x :: list -> (x :: list) :: (sufixes list)


let rec prefixes lst = 
  match lst with
  | [] -> [[]]
  | x :: list -> [] :: List.map (fun y -> x :: y) (prefixes list) ;;
  
let test_s = sufixes [1;2;3]
let test_p = prefixes [1;2;3];;
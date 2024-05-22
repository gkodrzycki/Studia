type tree =
  | Leaf
  | Node of tree * int * tree

let rec cbt h v =
  if h = 0
    then Leaf
    else let tmp = cbt (h-1) v in
      Node(tmp, v, tmp)
;;

module M = Map.Make(Int)

let split n =
  if n mod 2 = 1 
    then n/2, n/2
    else n/2 + 1, n/2
;;

let bt n v =
  let rec helper n v m = 
    begin match M.find_opt n m with 
    | Some(x) -> x, m
    | None when n = 0 ->
      let x = Leaf in 
      x, M.add n x m
    | None ->
      let (ln, rn) = split n in 
      let (lt, m) = helper ln v m in 
      let (rt, m) = helper rn v m in 
      let t = Node(lt, v, rt) in 
      t, M.add n t m
    end
  in fst (helper n v M.empty)
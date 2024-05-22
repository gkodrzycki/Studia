let  merge_ogon func a b = 
  let rec _merge a b acc = match a,b with
  | [],[] -> acc
  | x :: a, [] -> _merge  a [] (x :: acc)
  | [], x :: b -> _merge  [] b (x :: acc)
  | x :: a, y :: b -> if (func x y) then _merge a (y :: b) (x :: acc) else _merge (x :: a) b (y :: acc)
in _merge a b []

let rec halve list = 
  match list with
  | [] -> [[];[]]
  | x :: [] -> [[x];[]]
  | x :: y :: list -> (fun z -> [x :: (List.hd(z));y :: (List.hd(List.tl(z)))]) (halve list)

let rec merge_ogon_sort cmp a = 
  match (halve a) with
  | [[];[]] -> []
  | [[x];[]] -> [x]
  | [[];[y]] -> [y]
  | [x; y] -> let new_cmp = (fun a b -> not (cmp a b))
  in merge_ogon new_cmp (merge_ogon_sort new_cmp x) (merge_ogon_sort new_cmp y)
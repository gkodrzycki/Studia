let rec fold_left_cps (f : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list)  (cont : 'a -> 'c) =
  match xs with 
  | [] -> cont acc
  | x :: xs -> f acc x (fun newacc -> fold_left_cps f newacc xs cont)

let for_all xs pred =
  let f acc elem cont = 
    if pred elem then cont acc else false in
  fold_left_cps f true xs (fun x -> x)


let mult_list xs =
  let f acc elem cont = 
    if elem <> 0 then cont(acc*elem) else 0 in
  fold_left_cps f 1 xs (fun x -> x)
  

let sorted xs =
  let f = (fun (acc, prev) elem cont ->
    begin match prev with
    | None -> cont(acc, Some elem)
    | Some p -> if p < elem then cont(acc, (Some elem)) else (false, None) end) in  
  fold_left_cps f (true, None) xs (fun x -> x) |> fst


(* Testy  *)
let test_list1 = [2;3;4]
let test_list2 = [2;0;4]

let pred x = if x > 1 then true else false 


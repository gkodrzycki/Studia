type _ fin_type = 
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a*'b) fin_type

let fold_right (func : 'a -> 'b -> 'b) (lista : 'a list) (acc : 'b) =
  let rec _fold_right func lista acc = 
    match lista with 
    | [] -> acc
    | x :: xs -> _fold_right func xs (func x acc)
  in _fold_right func lista acc;;

let combine a b = 
  let rec _combine a b acc = 
    match a with
    | [] -> acc
    | x :: xs ->_combine xs b ((fold_right (fun elem acc -> (x,elem) :: acc) b []) :: acc)
  in List.flatten (_combine a b []);;
  
let rec _all_value : type a. a fin_type -> a list =
  fun ty ->
    match ty with 
    | Unit -> [()]
    | Bool -> [true;false]
    | Pair (tp1,tp2) -> combine (_all_value tp1) (_all_value tp2);;
  

let all_value : type a. a fin_type -> a Seq.t = 
  fun ty -> List.to_seq (_all_value ty);; 

let prod_sing_seq elem sequence = 
  Seq.fold_left (fun x y -> Seq.cons (elem,y) x) Seq.empty sequence;;

let prod sq1 sq2 =
  Seq.fold_left (fun x y -> Seq.append (prod_sing_seq y sq2) x) Seq.empty sq1;;

let all_value : type a. a fin_type -> a Seq.t = 
  fun ty -> 
  match ty with 
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair (tp1,tp2) -> prod (all_value tp1) (all_value tp2);;
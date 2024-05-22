type empty = |;;
type _ fin_type = 
| Empty : empty fin_type
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a*'b) fin_type
| Either : 'a fin_type * 'b fin_type -> ('a,'b) Either.t fin_type
| Function : 'a fin_type * 'b fin_type -> ('a * 'b) List.t fin_type;;


let prod_sing_seq elem sequence = 
  Seq.fold_left (fun x y -> Seq.cons (elem,y) x) Seq.empty sequence;;

let prod sq1 sq2 =
  Seq.fold_left (fun x y -> Seq.append (prod_sing_seq y sq2) x) Seq.empty sq1;;

let gen_space sq1 sq2 = 
  Seq.fold_left (fun x y -> Seq.cons (prod_sing_seq y sq2) x) Seq.empty sq1;;

let gen_value elem sq = 
  Seq.fold_left (fun x y -> Seq.cons (Seq.cons elem y) x) Seq.empty sq;;

let rec _gen_func sq origin acc = 
  match sq with 
  | Seq.Nil -> acc
  | Seq.Cons (x,xs) -> _gen_func (xs ()) origin (Seq.append (gen_value x origin) acc);;

let gen_func sq1 sq2 = 
  let value = gen_space sq1 sq2 in
  let rec __gen_func sq acc = 
    match sq with
    | Seq.Nil -> acc
    | Seq.Cons (x,xs) -> __gen_func (xs ()) (_gen_func x acc Seq.empty)
  in match value () with
  | Seq.Nil -> failwith "Empty"
  | Seq.Cons (x,xs) -> 
  Seq.fold_left (fun x y -> Seq.append (__gen_func ((Seq.map (fun x -> x()) xs) ()) (Seq.return (Seq.return y))) x) Seq.empty x;; 

let rec all_value : type a. a fin_type -> a Seq.t = 
  fun ty -> 
  match ty with 
  | Empty -> Seq.empty
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair (tp1,tp2) -> prod (all_value tp1) (all_value tp2)
  | Either (tp1,tp2) ->  Seq.append 
  (Seq.map (fun x-> Either.Left x) (all_value tp1)) 
  (Seq.map (fun x-> Either.Right x) (all_value tp2))
  | Function (tp1,tp2) -> let x = all_value tp1 in let y = all_value tp2 in
  let z = gen_func x y in (Seq.map List.of_seq z);;

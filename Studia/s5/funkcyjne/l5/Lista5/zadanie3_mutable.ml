type fraction = {numerator : int; denumerator : int};;

type node = {value : fraction; 
mutable left : fraction_tree; 
mutable right : fraction_tree} and
fraction_tree = 
| Leaf of {from : fraction; _to : fraction}
| Node of node ;;

let mediant (a : fraction) (b : fraction) : fraction = {numerator = a.numerator + b.numerator; denumerator = a.denumerator + b.denumerator};;

let create_frac (a:int) (b:int) : fraction = 
  if a < 0 then failwith "Negative value"
  else if b < 0 then failwith "Negative value" 
  else {numerator = a; denumerator = b};;

let create_tree () = Leaf {from = (create_frac 0 1); _to = (create_frac 1 0)};;

let create_tree_param (a : fraction) (b : fraction) = Leaf {from = a; _to = b};;


let go_left (tree : fraction_tree) : fraction_tree =
  match tree with
  | Leaf {from;_to} ->  Leaf {from;_to}
  | Node {value;left;right} -> left;;

let go_right (tree : fraction_tree) : fraction_tree =
  match tree with
  | Leaf {from;_to} ->  Leaf {from;_to}
  | Node {value;left;right} -> right;;

let split_left (tree : fraction_tree) : fraction_tree = 
  match tree with 
  | Leaf _ -> failwith "Leaf"
  | Node a -> 
    match a.left with 
    | Node _ -> failwith "Cannot split already splitted leaf"
    | Leaf {from; _to} -> let x = mediant from _to in
      let y = Node {value = x; left = create_tree_param from x; right = create_tree_param x _to}
      in (a.left <- y); y;;

let split_right (tree : fraction_tree) : fraction_tree = 
  match tree with 
  | Leaf _ -> failwith "Leaf"
  | Node a -> 
    match a.right with  
    | Node _ -> failwith "Cannot split already splitted leaf"
    | Leaf {from; _to} -> let x = mediant from _to in
      let y = Node {value = x; left = create_tree_param from x; right = create_tree_param x _to}
      in (a.right <- y); y;;


let split (tree : fraction_tree) : fraction_tree = 
  match tree with 
  | Leaf {from;_to} -> let x = mediant from _to in 
  Node {value = x;
  left = Leaf{from = from; _to = x};
   right = Leaf {from = x; _to = _to}}
  | _ -> failwith "Unkown";;

let nice_print frac = Printf.printf "%i/%i\n" frac.numerator frac.denumerator;;
let rec nice_print_tree tree =
  match tree with 
  | Node a -> 
    (nice_print_tree a.left) ; nice_print a.value ; nice_print_tree a.right
  | Leaf _ -> ();;

let rec go_right_max tree = 
match tree with
| Leaf _ -> failwith "Leaf"
| Node a as n -> 
  match a.right with 
  | Node _ -> go_right_max a.right
  | Leaf _ -> n;;

let rec go_left_max tree = 
  match tree with 
  | Leaf _ -> failwith "Leaf"
  | Node a as n -> 
    match a.left with 
    | Node _ -> go_left_max a.left
    | Leaf _ -> n;;

let compare (a : fraction) (b : fraction) =
  a.numerator * b.denumerator < b.numerator * a.denumerator;;

let equal (a : fraction) (b : fraction) =
  a.denumerator == b.denumerator && a.numerator == b.numerator;;

let rec get_value fraction tree = 
  match tree with
  | Leaf _ -> failwith "Use split"
  | Node v as n -> 
    if equal v.value fraction then n
    else if compare fraction v.value 
    then begin
      match v.left with
      | Node _ -> get_value fraction v.left
      | Leaf _ -> let x = split_left n in get_value fraction x
    end
    else begin
      match v.right with 
      | Node _ -> get_value fraction v.right
      | Leaf _ -> let x = split_right n in get_value fraction x
    end ;; 

let path fraction tree =
  let rec _path fraction tree acc =
  match tree with
  | Leaf _ -> failwith "Unkown"
  | Node v -> 
    if equal v.value fraction then acc ^ "end"
    else if compare fraction v.value
    then (_path fraction v.left (acc ^ "left "))
    else (_path fraction v.right (acc ^ "right " )) 
  in _path fraction tree "";;  
    

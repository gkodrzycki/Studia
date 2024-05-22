type fraction = {numerator : int; denumerator : int};;
type fraction_tree = 
| Leaf of {from : fraction; _to : fraction}
| Node of {value : fraction; left : fraction_tree; right : fraction_tree};;

let mediant (a : fraction) (b : fraction) : fraction = {numerator = a.numerator + b.numerator; denumerator = a.denumerator + b.denumerator};;

let create_frac (a:int) (b:int) : fraction = 
if a < 0 then failwith "Negative value"
else if b < 0 then failwith "Negative value" 
else {numerator = a; denumerator = b};;

let create_tree () = Leaf {from = (create_frac 0 1); _to = (create_frac 1 0)};;

let go_left (tree : fraction_tree) : fraction_tree =
  match tree with
  | Leaf {from;_to} ->  Leaf {from;_to}
  | Node {value;left;right} -> left;;

let go_right (tree : fraction_tree) : fraction_tree =
  match tree with
  | Leaf {from;_to} ->  Leaf {from;_to}
  | Node {value;left;right} -> right;;

let split (tree : fraction_tree) : fraction_tree = 
  match tree with 
  | Leaf {from;_to} -> let x = mediant from _to in 
  Node {value = x;
  left = Leaf{from = from; _to = x};
   right = Leaf {from = x; _to = _to}}
  | _ -> failwith "Unkown";;
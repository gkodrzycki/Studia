type 'a lazy_tree = Leaf | Node of 'a t * 'a * 'a t
and 'a t = unit -> 'a lazy_tree

let rec qtree (a, b) (c, d) () =
  Node (qtree (a, b) (a + c, b + d), (a + c, b + d), qtree (a + c, b + d) (c, d))

let rec generate depth qtree =
  if depth > 0 then (
    match qtree with
    | Leaf -> print_endline ""
    | Node (l, (x, y), r) ->
        Printf.printf "%d / %d\n" x y;
        generate (depth - 1) (l ());
        generate (depth - 1) (r ()))
  else ()


let start = qtree (0, 1) (1, 0) ()


(* Test *)
generate 2 start;;

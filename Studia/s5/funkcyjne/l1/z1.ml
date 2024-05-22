(*No ma typ a' -> a' ale jak zrobić int -> int? *)

(* 1. można oszukać *)
let id_int = fun x : int -> x ;;

(* 2. można wymusić typy korzystając z +*)
let id_int = fun x -> x + 0 ;;


(* (’a -> ’b) -> (’c -> ’a) -> ’c -> ’b *)
let compose x y z = x(y(z)) ;;
(* 
  x: 'a -> 'b
  y: 'c -> 'a 
  z: 'c    
*)

(* ’a -> ’b -> ’a *)
let first x y = x;;

(* ’a -> ’a -> ’a *)
let g x y = if true then x else y ;;  

(* Korzystamy z listy 0*)

(* I chyba ciężko o a'*)
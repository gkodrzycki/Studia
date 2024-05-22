let id_int = fun x : int -> x ;;


(* 'a -> 'a *)

let id_int = fun x -> x + 0 ;;




let zlozenie x y z = x( y( z ) ) ;;
            (* x: 'a -> 'b
            y: 'c -> 'a 
            z: 'c    *)

let first x y = x;;

let f x x = x;;

let g x y = if true then x else y ;;  

let x = 1

let _ = x
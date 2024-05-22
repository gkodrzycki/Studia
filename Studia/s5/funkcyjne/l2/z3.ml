(* Zadanie 3 *)

let rec suffixes acc lst = 
    match lst with
    | [] -> [] :: acc
    | _ :: tl -> suffixes (lst :: acc) tl ;;


let rec suffixes2 lst =
    match lst with
    | [] -> [[]]
    | _ :: tl -> lst :: suffixes2 tl ;;

let rec prefixes lst = 
    match lst with
    | [] -> [[]]
    | hd :: tl -> [] :: List.map (fun x -> hd :: x) (prefixes tl) ;;


(* Zadanie 1 *)
let length list = List.fold_left (fun x y -> x + 1 ) 0 list ;;

let rev list = List.fold_left (fun acc el -> el :: acc) [] list ;;

let map f list = List.fold_right (fun x y -> (f x) :: y) list [] ;;

let append list1 list2 = List.fold_right (fun el acc -> el :: acc) list1 list2 ;;

let rev_append list1 list2 = List.fold_left (fun acc el -> el :: acc) list2 list1 ;;

let filter f lst = List.fold_right (fun el acc -> if (f el) then el :: acc else acc) lst  [];;

let rev_map f lst = List.fold_left (fun acc el -> (f el) :: acc) [] lst ;;

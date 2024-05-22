(* Zadanie 2 *)

let rec sublist acc res lst = 
    match lst with
    | [] -> acc :: res
    | hd :: tl -> 
        (sublist acc 
            (sublist (hd :: acc) res tl) tl)

let sublists lst = 
    sublist [] [] (List.rev lst)

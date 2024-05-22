(* Zadanie 4 *)

let rec merge cmp alist blist =
    match alist, blist with
    | [], [] -> []
    | [], hd :: tl | hd :: tl, [] -> hd :: tl
    | ahd :: atl, bhd :: btl -> if cmp ahd bhd
                                 then ahd :: (merge cmp atl blist)
                                 else bhd :: (merge cmp alist btl) ;;

let merge_tail cmp alist blist =
    let rec _merge_tail cmp res alist blist = 
        match alist, blist with
        | [], [] -> res
        | [], hd :: tl | hd :: tl, [] -> res @ (hd :: tl)
        | ahd :: atl, bhd :: btl -> if cmp ahd bhd
                                 then _merge_tail cmp (res @ [ahd]) atl blist
                                 else _merge_tail cmp (res @ [bhd]) alist btl
    in _merge_tail cmp [] alist blist ;;

let halve lst = 
    let rec _halve acc bcc lst = 
        match lst with
        | [] -> acc, bcc
        | hd :: [] -> (hd :: acc), bcc
        | fst :: snd :: tl -> (_halve (fst :: acc) (snd :: bcc) tl) in _halve [] [] (List.rev lst);;

let rec merge_sort cmp lst =
    match halve lst with
    | [a], [] | [], [a] -> [a]
    | alst, blst -> merge cmp (merge_sort cmp alst) (merge_sort cmp blst) ;;


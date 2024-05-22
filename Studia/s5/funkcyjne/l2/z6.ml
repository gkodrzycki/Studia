(* Zadanie 6 *)

let perm lst = 
    let rec _perm acc result lst_filter lst_iter =
        match lst_iter,lst_filter with
        | [], [] -> acc :: result
        | [], hd :: tl -> result
        | ihd :: itl, _ -> 
                let filtered = (List.filter ((!=) ihd) lst_filter) in 
                _perm acc (_perm (ihd :: acc) result filtered filtered) lst_filter itl
    in _perm [] [] lst lst ;;



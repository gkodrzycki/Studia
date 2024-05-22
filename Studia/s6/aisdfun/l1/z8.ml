let append1 xs ys =
  let rec _append_nieogon xs ys =
    match xs with
    | [] -> ys
    | (x :: xs) -> x :: (_append_nieogon xs ys) in
  _append_nieogon xs ys

let append2 xs ys =
  let rec _append_ogon xs acc =
    match xs with
    | [] -> acc
    | (x :: xs) -> _append_ogon xs (x :: acc) in
  _append_ogon (List.rev xs) ys
    
(* Ale to nie jest solve btw *)
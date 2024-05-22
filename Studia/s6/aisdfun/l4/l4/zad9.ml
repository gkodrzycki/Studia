open Stream

let min cmp xs =
  match xs with
  | (lazy (Cons (el, s))) ->
      Some (foldl (fun acc e -> if cmp acc e then acc else e) s el)
  | (lazy Nil) -> None

let select cmp xs =
  match min cmp xs with
  | None -> None
  | Some e -> Some (e, dropOne (fun a -> a = e) xs)

let rec select_sort cmp xs =
  match select cmp xs with
  | None -> lazy Nil
  | Some (e, xs) -> lazy (Cons (e, select_sort cmp xs))

(* Przykład *)
let a =
  lazy
    (Cons
       ( 5,
         lazy
           (Cons
              (10, lazy (Cons (7, lazy (Cons (5, lazy (Cons (1, lazy Nil))))))))
       ))

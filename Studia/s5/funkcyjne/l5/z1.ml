let rec fix_with_limit rec_lim f x =
  if rec_lim > 0 then f (fix_with_limit (rec_lim - 1) f) x
  else failwith "recursion depth exceeded"

let fix_memo f x =
  let tbl = Hashtbl.create 30 in
  let rec _fix f x =
    match Hashtbl.find_opt tbl x with
    | Some x -> x
    | None ->
        let res = f (_fix f) x in
        Hashtbl.add tbl x res;
        res
  in
  _fix f x


let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib = fix fib_f

(* Testy *)
fix_with_limit 100 fib 12;;
fix_with_limit 10000 fib_f 40;;
fix_memo fib_f 40;;
fix_memo fib_f 70;;

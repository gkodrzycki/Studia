(* Definicje funkcji *)
let rec tabulate ?(l = 0) r stream = if r == l then [] else  (stream l) :: (tabulate ~l:(l+1) r stream)

(* Testy *)

let rec stream_example x = if x == 0 then 0 else stream_example (x - 1) + 1 (* ciąg 0,1,2,3 *)


let check  = tabulate ~l:0 5 stream_example 
let check2 = tabulate ~l:0 0 stream_example                             
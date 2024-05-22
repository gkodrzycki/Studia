let ctrue = fun x -> fun y -> if true then x else y
let cfalse = fun x -> fun y -> if false then x else y

(* let cand = fun x -> fun y -> if x == ctrue && y == x then x else cfalse
let cor = fun x -> fun y -> if x == cfalse && y == x then cfalse else x *)

let cand val1 val2 = (val1 (val2 ctrue cfalse) cfalse)
let cor val1 val2 = (val1 ctrue (val2 ctrue cfalse))

let test = (cand ctrue ctrue) == ctrue
let test2 = (cand cfalse ctrue) == cfalse

let cbool_of_bool = fun x -> if x then ctrue else cfalse
let bool_of_cbool = fun x -> if (x true false) then true else false


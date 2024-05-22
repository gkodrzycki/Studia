type cbool = { cbool : 'a. 'a -> 'a -> 'a }

let ctrue : cbool = {cbool = fun x -> fun y -> if true then x else y}
let cfalse : cbool = {cbool = fun x -> fun y -> if false then x else y}

let cand (x : cbool) (y : cbool): cbool = {cbool = if x == ctrue && y == x then x.cbool else cfalse.cbool}
let cor (x : cbool) (y : cbool) : cbool = {cbool = if x == ctrue then x.cbool else y.cbool}

let cbool_of_bool (x : bool) : cbool = {cbool = if x == true then ctrue.cbool else cfalse.cbool}
let bool_of_cbool (x : cbool) : bool = if x == ctrue then true else false

type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let zero : cnum = {cnum = fun f x -> if true then x else f x}

let succ value : cnum = {cnum = fun f x -> if value == zero then f(value.cnum f x) else f(value.cnum f x)}

let rec int_of_cnum (n : cnum) : int = if n == zero then 0 else n.cnum (fun x -> x+1) 0

let rec cnum_of_int (n : int) : cnum = if n == 0 then zero else (succ(cnum_of_int(n-1)))

let mult (val1 : cnum) (val2 : cnum) : cnum = {cnum = fun f x -> if val1 == zero || val2 == zero then f(val1.cnum(val2.cnum(f)) x) else f(val1.cnum(val2.cnum(f)) x) }

let add (val1 : cnum) (val2 : cnum) : cnum = {cnum = fun f x -> if val1 == zero || val2 == zero then f(val1.cnum f ((val2.cnum) f x)) else  f(val1.cnum f ((val2.cnum) f x)) }

let is_zero (n : cnum) : cbool = {cbool = if n == zero then ctrue.cbool else cfalse.cbool}

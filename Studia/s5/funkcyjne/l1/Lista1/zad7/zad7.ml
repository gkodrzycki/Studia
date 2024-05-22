let ctrue = fun x -> fun y -> if true then x else y
let cfalse = fun x -> fun y -> if false then x else y

let zero f x= if true then x else f x
let succ value f x = if value == zero then f(value f  x) else  f(value f  x)
let rec int_of_cnum n = if n == zero then 0 else n (fun x -> x+1) 0
let rec cnum_of_int n = if n == 0 then zero else succ(cnum_of_int(n-1))
let mult val1 val2 f x = if val1 == zero || val2 == zero then f(val1(val2(f)) x) else f(val1(val2(f)) x) 
let add val1 val2 f x = if val1 == zero || val2 == zero then f(val1 f ((val2) f x)) else  f(val1 f ((val2) f x))
let is_zero n = if n == zero then ctrue else cfalse

let test = (succ (succ (succ (zero))))
let test2 = int_of_cnum test

let test3 = is_zero zero == ctrue
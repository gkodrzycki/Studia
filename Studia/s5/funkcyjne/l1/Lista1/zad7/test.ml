let zero = fun f x -> x
let succ value = fun f x -> f(value f  x)
let rec int_of_cnum n = if n == zero then 0 else n (fun x -> x+1) 0
let rec cnum_of_int n = if n == 0 then zero else succ(cnum_of_int(n-1))
let mult val1 val2 = fun f x -> val1(val2(f)) x
let add val1 val2 = fun f x -> (val1 f ((val2) f x))
let is_zero n = if n == zero then ctrue else cfalse

let test = (succ(succ(succ(zero))))
let wyn = int_of_cnum test
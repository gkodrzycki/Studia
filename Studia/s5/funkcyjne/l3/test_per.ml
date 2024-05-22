module Perm = Perm.Make(Int)
module Gen = Gen.Make(Perm)

let z = Perm.swap 2 3
let y = Perm.swap 3 4
let x = Perm.swap 4 5

let v = Perm.invert (Perm.compose (Perm.compose z y) x)
let l = [z ; y ]

let v = Gen.is_generated v l

let _ = if v then print_endline "WIN" else print_endline "LOSE :<"


let v2 = Perm.compose (Perm.compose z y) z
let l2 = [z ; y ]

let v2 = Gen.is_generated v2 l

let _ = if v2 then print_endline "WIN" else print_endline "LOSE :<"
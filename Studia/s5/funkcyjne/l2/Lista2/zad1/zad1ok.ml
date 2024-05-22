
let length x = List.fold_left (fun x y -> x+1) 0 x 
let test_length = assert(length [1;2;3] == 3)

let rev x = List.fold_left (fun x y -> y :: x) [] x 
let test_rev = rev [1;2;3]

let map x f = List.fold_right (fun y x -> (f y) :: x) x []
let test_map = map [1;2;3] (fun x -> x+1)

let append x y = List.fold_right (fun x y -> x :: y) x y
let test_append = append [1;2] [3;4] 

let rev_append x y = List.fold_left (fun y x -> x :: y) y x
let test_rev_append = rev_append [1;2] [3;4] 

let filter x f = List.fold_right (fun y x -> if (f y) == None then x else (f y) :: x) x []
let test_filter = filter [-1;2;-3;3;-4;5;6] (fun x -> if x >= 0 then Some x else None)

let rev_map x f = List.fold_left (fun x y -> (f y) :: x) [] x
let test_rev_map = rev_map [1;2;3] (fun x -> x+1)

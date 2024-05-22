let for_all xs pred =
  let f acc elem = 
    if pred elem then acc else failwith "Wartosc nie spelnia predykatu" in
  List.fold_left f true xs


let mult_list xs =
  let f acc elem = 
    if elem <> 0 then acc*elem else failwith "0 fajrant" in
  List.fold_left f 1 xs
  

let sorted xs =
  let f acc elem = 
    if acc < elem then elem else failwith "Nie rośnie??" in
  let _ = List.fold_left f Int.min_int xs in true


(* Testy  *)
let test_list1 = [2;3;4]
let test_list2 = [2;0;4]
let pred x = if x > 1 then true else false 


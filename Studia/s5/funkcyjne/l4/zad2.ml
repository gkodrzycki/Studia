

type 'a zlist =
  | Zlist of 'a list * 'a list

let of_list : 'a. 'a list -> 'a zlist =
  fun x ->
    Zlist([], x)

let to_list : 'a. 'a zlist -> 'a list =
  fun xs ->
    match xs with
    | Zlist(left, right) -> List.rev(left) @ right

let elem : 'a. 'a zlist -> 'a option =
  fun xs ->
    match xs with
    | Zlist(left, right) -> match right with
      | [] -> None
      | (x :: xs) -> Some x

let move_left : 'a. 'a zlist -> 'a zlist =
  fun xs -> 
    match xs with
    | Zlist(left, right) -> match left with
      | [] -> failwith "Popsute:("
      | (x :: xs) -> Zlist(xs, x :: right)

let move_right : 'a. 'a zlist -> 'a zlist =
  fun xs -> 
    match xs with
    | Zlist(left, right) -> match right with
      | [] -> failwith "Popsute:("
      | (x :: xs) -> Zlist(x :: left, xs)

let insert : 'a. 'a -> 'a zlist -> 'a zlist =
  fun x xs -> 
    match xs with
    | Zlist(left, right) -> Zlist(x :: left, right)

let remove : 'a. 'a zlist -> 'a zlist =
  fun xs -> 
    match xs with
    | Zlist(left, right) -> match left with
    | [] -> failwith "Popsute:("
    | (x :: xs) -> Zlist(xs, right)


let test_list = [1;2;3;4]
let test_zlist = of_list test_list

let test_elem = elem test_zlist

(* let test_left = move_left test_zlist *)

let test_right = move_right test_zlist
let test_right_elem = elem test_right

let test_insert = insert 10 test_right
let test_insert_elem = elem (move_left test_insert)

let test_remove = remove test_insert

let test_to_list = to_list test_insert

                
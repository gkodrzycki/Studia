type 'a dllist = 'a dllist_data Lazy.t
and 'a dllist_data = {
  prev : 'a dllist;
  elem : 'a;
  next : 'a dllist;
}

let prev (lista : 'a dllist) : 'a dllist =
  match lista with
  | lazy {prev;elem;next} -> prev;;

let next (lista : 'a dllist) : 'a dllist =
  match lista with
  | lazy {prev;elem;next} -> next;;

let elem (lista : 'a dllist) : 'a = 
  match lista with
  | lazy {prev;elem;next} -> elem;;

let singleton elem = 
  let rec s = lazy {next = s; elem; prev = s;} in s;;

let rec generate (previous : 'a dllist) lista first = 
  match lista with
  | [] -> first , previous
  | (x::lista) -> let last = ref (singleton x) in let rec
    node = lazy begin let (first,ending) = generate node lista first in 
    last := ending; {prev = previous; elem = x; next = first} end in
    let return = (Lazy.force node) in lazy return, !last ;;

let make_cycle lista = 
  let rec first = lazy begin let (beginning,last) = generate first (List.tl lista) first in 
  {prev = last; elem = List.hd lista; next = beginning} end in first;;   

let dllist_of_list (lista : 'a list) : 'a dllist = 
  match lista with 
  | [] -> failwith "Empty list. Cannot create a cyclic list."
  | _ -> make_cycle lista;;

let x = dllist_of_list [1;2;3];;
assert ((prev (next x)) == (next (prev x)));;

let rec left_int value previous =
  let rec node = lazy {prev = left_int (value-1) node ; elem = value; next = previous} in node;;

let rec right_int value previous=
  let rec node = lazy {prev = previous; elem = value; next = right_int (value+1) node} in node;;

let rec integers = lazy {prev = left_int (-1) integers; elem = 0; next = right_int 1 integers}
type 'a nlist =
  | Nil
  | Zero of ('a * 'a) nlist
  | One  of 'a * ('a * 'a) nlist
  | Two of 'a * 'a * ('a * 'a) nlist 


let rec cons : 'a. 'a -> 'a nlist -> 'a nlist =
  fun x xs ->
  match xs with
  | Nil        -> One(x, Nil)
  | Zero xs    -> One(x, xs)
  | One(y, xs) -> Two (x, y, xs)
  | Two(y, z, xs) -> One(x, cons (y, z) xs)

let rec view : 'a. 'a nlist -> ('a * 'a nlist) option =
  function
  | Nil -> None
  | One(x, xs) -> Some(x, Zero xs)
  | Two(x,y,xs) -> Some(x, One(y,xs))
  | Zero xs ->
    begin match view xs with
    | None -> None
    | Some((x, y), xs) -> Some(x, One(y, xs))
    end

let rec nth : 'a. 'a nlist -> int -> 'a =
  fun xs n ->
  match xs with
  | Nil -> raise Not_found
  | Zero xs ->
    let (x, y) = nth xs (n / 2) in
    if n mod 2 = 0 then x
    else y
  | One(x, xs) ->
    if n = 0 then x
    else nth (Zero xs) (n-1)
  | Two(x, y, xs) ->
    if n = 0 then x
    else nth (One (y, xs)) (n-1) 

let rec genlist n = 
  match n with
  | 0 -> Nil
  | n -> cons n (genlist (n-1))

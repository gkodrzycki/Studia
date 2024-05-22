module MyArr : sig
  type 'a array 
  val empty : 'a array
  val sub : 'a array -> int -> 'a option
  val update : 'a array -> int -> 'a -> 'a array

end = struct
  type 'a array = 
  | Empty
  | Pair of 'a * 'a array * 'a array
  | NonePair of 'a array * 'a array

  let empty = Empty

  let rec sub array idx =
    match array with
    | Empty -> None
    | Pair(v, left, right) ->
      if idx = 0 then Some v
      else if idx mod 2 = 1 then sub right (idx/2)
      else sub left (idx/2)
    | NonePair (left, right) -> 
      if idx = 0 then None 
      else if idx mod 2 = 1 then sub right (idx/2)
      else sub left (idx/2)


  let rec update array idx v = 
    match array with
    | Empty -> if idx = 0 
      then Pair (v, Empty, Empty)
      else if idx mod 2 = 1 then NonePair (Empty, update Empty (idx / 2) v)
      else NonePair (update Empty (idx/2) v, Empty)
    | Pair (value, left, right) ->
      if idx = 0 then Pair (v, left, right)
      else if idx mod 2 = 1 then Pair (value, left, update right (idx / 2) v)
      else Pair (value, update left (idx / 2) v, right)
    | NonePair (left, right) -> 
      if idx mod 2 = 1 then NonePair (left, update right (idx / 2) v)
      else NonePair (update left (idx/2) v, right) 
end 

let a = MyArr.empty;;
let a_updated = MyArr.update a 0 "val";;
let a_sub = MyArr.sub a_updated 0;;
let a_updated2 = MyArr.update a 0 "val1";;
let a_sub2 = MyArr.sub a_updated2 0;;
let a_updated3 = MyArr.update a 1 "val2";;
let a_sub3 = MyArr.sub a_updated3 1;;
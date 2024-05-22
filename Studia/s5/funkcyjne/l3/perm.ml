module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  (** permutacja jako funkcja *)
  val apply : t -> key -> key
  (** permutacja identycznościowa *)
  val id : t
  (** permutacja odwrotna *)
  val invert : t -> t
  (** permutacja która tylko zamienia dwa elementy miejscami *)
  val swap : key -> key -> t
  (** złożenie permutacji (jako złożenie funkcji) *)
  val compose : t -> t -> t
  (** porównywanie permutacji *)
  val compare : t -> t -> int
end

module Make(Key : OrderedType) =
struct
  module MyMap = Map.Make(Key)
  type key = Key.t
  type t = {p : key MyMap.t; r : key MyMap.t}

  let (id : t) = {p =  MyMap.empty ; r = MyMap.empty}

  let apply (perm : t) (key : Key.t) = match (MyMap.find_opt key perm.p) with
  | Some v -> v 
  | None -> key

  let invert perm = match perm with
  | {p = a; r = b} -> {p = b; r = a}

  let swap a b = 
    if Key.compare a b = 0
    then
        id
    else
      let perm = MyMap.add b a (MyMap.add a b MyMap.empty) in 
      let rev = MyMap.add b a (MyMap.add a b MyMap.empty) in 
      {p = perm; r = rev}

  let compose (perm1 : t) (perm2 : t) = 
    let perm_compose key x y = match x,y with
    | None,None -> None
    | None,Some y -> Some y
    | Some x, _ -> let res = apply perm2 x in 
      if res = key
        then
          None
        else
          Some res
    in
    let perm_inv_compose key x y = match x,y with
    | None,None -> None
    | Some y,None -> Some y
    | _ , Some x -> let res = apply (invert perm1) x in 
      if res = key
        then
          None
        else
          Some res
    in
    { p = MyMap.merge perm_compose perm1.p perm2.p; r = MyMap.merge perm_inv_compose perm1.r perm2.r}
    
    let compare (a : t) (b : t) =
      MyMap.compare Key.compare a.p b.p
end

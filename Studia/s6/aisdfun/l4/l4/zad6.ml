module InfDepthTree : sig
  type 'a treeCell = Node of 'a * 'a tree Stream.stream | Nil
  and 'a tree = 'a treeCell Lazy.t

  val constTree : 'a -> 'a tree
  val tMap : ('a -> 'b) -> 'a tree -> 'b tree
  val subTree : int list -> 'a tree -> 'a tree
  val mirror : 'a tree -> 'a tree
end = struct
  type 'a treeCell = Node of 'a * 'a tree Stream.stream | Nil
  and 'a tree = 'a treeCell Lazy.t

  let rec constTree x = lazy (Node (x, Stream.constStream (constTree x)))

  let rec tMap f t =
    match t with
    | (lazy (Node (x, tlst))) -> lazy (Node (f x, Stream.sMap (tMap f) tlst))
    | (lazy Nil) -> lazy Nil

  let rec subTree dlst t =
    match (dlst, t) with
    | x :: xs, (lazy (Node (e, lst))) -> subTree xs (Stream.nth x lst)
    | [], _ -> t
    | _, (lazy Nil) -> lazy Nil

  let rec mirror t =
    match t with
    | (lazy (Node (x, tlst))) -> lazy (Node (x, Stream.sRevMap mirror tlst))
    | (lazy Nil) -> lazy Nil
end

module BinInfDepthTree : sig
  type 'a treeCell = Node of 'a tree * 'a * 'a tree | Nil
  and 'a tree = 'a treeCell Lazy.t

  type dir = L | R

  val constTree : 'a -> 'a tree
  val tMap : ('a -> 'b) -> 'a tree -> 'b tree
  val subTree : dir list -> 'a tree -> 'a tree
  val mirror : 'a tree -> 'a tree
end = struct
  type 'a treeCell = Node of 'a tree * 'a * 'a tree | Nil
  and 'a tree = 'a treeCell Lazy.t

  type dir = L | R

  let rec constTree x =
    let a = lazy (constTree x) in
    lazy (Node (Lazy.force a, x, Lazy.force a))

  let rec mirror t =
    match t with
    | (lazy (Node (l, x, r))) -> lazy (Node (mirror r, x, mirror l))
    | (lazy Nil) -> lazy Nil

  let rec tMap f t =
    match t with
    | (lazy (Node (l, x, r))) -> lazy (Node (tMap f l, f x, tMap f r))
    | (lazy Nil) -> lazy Nil

  let rec subTree dlst t =
    match (dlst, t) with
    | L :: dlst, (lazy (Node (l, _, _))) -> subTree dlst l
    | R :: dlst, (lazy (Node (_, _, r))) -> subTree dlst r
    | [], (lazy (Node (_, _, _))) | _, (lazy Nil) -> t
end

let infTree =
  let rec _infTree n =
    lazy (BinInfDepthTree.Node (_infTree (2 * n), n, _infTree ((2 * n) + 1)))
  in
  _infTree 1

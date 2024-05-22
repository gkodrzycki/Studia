signature ORDERED =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED

  type Heap

  val empty : Heap
  val isEmpty : Heap -> bool

  val insert : Elem.T * Heap -> Heap
  val merge: Heap * Heap -> Heap

  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end


functor ExplicitHeap (H : HEAP) : HEAP =
struct
  structure Elem = H.Elem
  datatype Heap = E | NE of Elem.T * H.Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun insert (x, E) = NE (x, H.insert (x, H.empty))
    | insert (x, NE (y, h)) =
      if Elem.leq (x, y) then NE (x, H.insert (x, h)) else NE (y, H.insert (x, h))

  fun merge (h1, E) = h1
    | merge (E, h2) = h2
    | merge (NE (x1, h1), NE (x2, h2)) =
      if Elem.leq (x1, x2) then NE (x1, H.merge (h1, h2))
      else NE (x2, H.merge (h1, h2))

  fun findMin E = raise Empty
    | findMin (NE(x, _)) = x

  fun deleteMin E = raise Empty 
    | deleteMin (NE(_, h)) =
        let val newH = H.deleteMin h
    in
        if H.isEmpty newH then
            E
        else
            NE(H.findMin newH, newH)
    end
end 

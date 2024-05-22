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

functor BinomialHeap2 (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  type Heap = (int * Tree) list

  val empty = []
  fun isEmpty ts = null ts
  
  fun rank (r, _) = r
  fun root (_, (Node (x, c))) = x
  
  fun link (t1 as Node (x1, c1), t2 as Node (x2, c2)) =
    if Elem.leq (x1, x2) then Node (x1, t2::c1) 
    else Node (x2, t1::c2)
  
  fun insTree (t, []) = [t]
    | insTree ((r1, t1), ts as (r2, t2)::ts') =
    if r1 < r2 then (r1, t1)::ts
    else insTree ((r2 + 1, link (t1, t2)), ts')

  fun insert (x, ts) = insTree ((0, Node (x, [])), ts)
  
  fun merge (ts1, []) = ts1
    | merge ([], ts2) = ts2
    | merge (ts1 as (r1, t1)::ts1', ts2 as (r2, t2)::ts2') =
    if r1 < r2 then (r1, t1)::merge (ts1', ts2)
    else if r2 < r1 then (r2, t2)::merge (ts2', ts1)
    else insTree ((r1 + 1, link (t1, t2)), merge (ts1', ts2'))

  fun removeMinTree [] = raise Empty
        | removeMinTree [t] = (t, [])
        | removeMinTree (t :: ts) =
            let val (t', ts') = removeMinTree ts
            in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end
    
  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  
  fun deleteMin ts =
    let
      val ((r, Node (_, ts1)), ts2) = removeMinTree ts
      fun helper (r, n::ns) = (r, n)::helper (r + 1, ns)
    in merge (helper (0, rev ts1), ts2) end
end


structure IntOrdered : ORDERED =
struct
  type T = int
  fun eq (x, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

structure MyBinomialHeap = BinomialHeap2(IntOrdered)

(* Teściki *)
val myHeap = MyBinomialHeap.empty;
val myHeapWithElement1 = MyBinomialHeap.insert(5, myHeap);
val myHeapWithElement2 = MyBinomialHeap.insert(3, myHeapWithElement1);
val myHeapWithElement3 = MyBinomialHeap.insert(10, myHeapWithElement2)
val minimum = MyBinomialHeap.findMin(myHeapWithElement3);
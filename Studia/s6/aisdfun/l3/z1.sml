signature ORDERED =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

structure IntOrdered : ORDERED =
struct
  type T = int
  fun eq (x, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

functor BinomialHeap (Element : ORDERED) =
struct
    structure Elem = Element

    datatype Tree = Node of int * Elem.T * Tree list
    type Heap = Tree list

    val empty = []
    fun isEmpty ts = null ts

    fun rank (Node (r, x, c)) = r
    fun root (Node (r, x, c)) = x
    
    fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) = 
        if Elem.leq (x1, x2) then Node (r+1, x1, t2 :: c1)
        else Node (r+1, x2, t1 :: c1)

    fun insTree (t, []) = [t]
        | insTree (t, ts as t' :: ts') = 
        if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

    fun insert (x, ts) = insTree (Node (0, x, []), ts)
    fun merge (ts1, []) = ts1
        | merge ([], ts2) = ts2
        | merge (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
            if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
            else if rank t2 < rank t1 then t2 :: merge (ts1, ts2')
            else insTree (link (t1, t2), merge (ts1', ts2'))

    fun removeMinTree [] = raise Empty
        | removeMinTree [t] = (t, [])
        | removeMinTree (t :: ts) =
            let val (t', ts') = removeMinTree ts
            in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end
    
    (* oryginalne findMin *)
    (* fun findMin ts = let val (t, _) = removeMinTree ts in root t end  *)

    (* Podejście dzień dobry iterujmy się *)
    fun findMin [] = raise Empty 
        | findMin (t :: ts) = foldl (fn (x,acc) => if Elem.leq((root x), acc) 
                                                 then root x else acc) (root t) ts

    fun deleteMin ts =
        let val (Node (_, x, ts1), ts2) = removeMinTree ts
        in merge (rev ts1, ts2) end
end

structure MyBinomialHeap = BinomialHeap(IntOrdered)

(* Teściki *)
val myHeap = MyBinomialHeap.empty;
val myHeapWithElement1 = MyBinomialHeap.insert(5, myHeap);
val myHeapWithElement2 = MyBinomialHeap.insert(3, myHeapWithElement1);
val myHeapWithElement3 = MyBinomialHeap.insert(10, myHeapWithElement2)
val minimum = MyBinomialHeap.findMin(myHeap);
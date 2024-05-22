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

functor SplayHeap (Element: ORDERED) = 
struct
    structure Elem = Element

    datatype Heap = E | T of Heap * Elem.T * Heap

    val empty = E

    fun isEmpty E = true 
      | isEmpty _ = false

    fun partition (pivot, E) = (E, E)
      | partition (pivot, t as T(a, x, b)) =
        if Elem.leq (x, pivot) then
            case b of
              E => (t, E)
            | T(b1, y, b2) => 
                if Elem.leq (y, pivot) then 
                    let val (small, big) = partition(pivot, b2)
                    in (T (T(a, x, b1), y, small), big) end
                else
                    let val (small, big) = partition (pivot, b1)
                    in  (T(a, x, small), T(big, y, b2)) end
        else
            case a of 
              E => (E, t)
            | T(a1, y, a2) =>
                if Elem.leq (y, pivot) then
                    let val (small, big) = partition (pivot, a2)
                    in (T(a1, y, small), T(big, x, b)) end
                else
                    let val (small, big) = partition(pivot, a1)
                    in (small, T(big, y, T(a2, x, b))) end

    fun smaller (_, E) = E
      | smaller (pivot, (T(a, x, b))) = 
        if Elem.lt (pivot, x) then smaller (pivot, a)
        else 
            case b of 
              E => T(a, x, E)
            | T(b1, y, b2) => 
                if Elem.lt (pivot, y) then T(a, x, smaller (pivot, b1))
                else T(T(a, x, b1), y, smaller (pivot, b2))

    fun bigger (_, E) = E
      | bigger (pivot, (T(a, x, b))) = 
          if Elem.leq (x, pivot) then bigger (pivot, b)
          else 
              case a of 
              E => T(E, x,  b)
              | T(a1, y, a2) => 
                  if Elem.leq (y, pivot) then T(bigger(pivot, a2), x, b)
                  else T(bigger (pivot, a1), y, T(a2, x, b))

    fun insert (x,t) = let val (a, b) = partition (x, t) in T(a, x, b) end

    fun merge (E, t) = t
      | merge (T (a, x, b), t) = 
        let val (ta, tb) = partition (x, t)
        in T(merge (ta, a), x, merge(tb, b)) end

    fun findMin E = raise Empty
      | findMin (T(E, x, b)) = x
      | findMin (T(a, x, b)) = findMin a

    fun deleteMin E = raise Empty
      | deleteMin (T(E, x, b)) = b
      | deleteMin (T (T(E,x,b), y, c)) = T(b, y, c)
      | deleteMin (T (T(a,x,b), y, c)) = T (deleteMin a, x, T(b,y,c))
end


structure IntOrdered : ORDERED =
struct
  type T = int
  fun eq (x, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

(* coś w ramach testu *)


structure IntHeap = SplayHeap(IntOrdered)

(* val tree = MyHeap.insert (5, MyHeap.empty)
val tree = MyHeap.insert (2, tree)
val tree = MyHeap.insert (8, tree)
val tree = MyHeap.insert (1, tree)
val tree = MyHeap.insert (3, tree)
val tree = MyHeap.insert (6, tree)
val tree = MyHeap.insert (9, tree) *)


val tree = IntHeap.T(IntHeap.E, 1, IntHeap.T(IntHeap.T(IntHeap.E,3,IntHeap.E), 4, IntHeap.T(IntHeap.E, 5, IntHeap.E)))

val (left, right) = IntHeap.partition(2, tree)


val smaller_tree = IntHeap.smaller(2, tree)
val bigger_tree = IntHeap.bigger(2, tree)
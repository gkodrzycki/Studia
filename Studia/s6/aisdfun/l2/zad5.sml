signature Ordered =
sig 
    type T 
    val eq  : T * T -> bool
    val lt  : T * T -> bool
    val leq : T * T -> bool
end

structure Ordered_Ints : Ordered =
struct
  type T = int
  fun lt (a,b) = a < b
  fun eq (a,b) = a = b
  fun leq (a,b) = a <= b
end

signature Heap =
sig 
    structure Elem : Ordered

    type Heap

    val empty       : Heap
    val isEmpty     : Heap -> bool

    val insert      : Elem.T * Heap -> Heap
    val merge       : Heap * Heap -> Heap

    val findMin     : Heap -> Elem.T 
    val deleteMin   : Heap -> Heap
end

functor LeftistHeap (Element : Ordered) : Heap =
struct 
    structure Elem = Element
    
    datatype Heap = E | T of int * Elem.T * Heap * Heap

    fun rank E = 0
        | rank (T (r, _, _, _)) = r 
    fun makeT (x, a, b) = if rank a >= rank b then T(rank b + 1, x, a, b) 
                          else T(rank a + 1, x, b, a)

    val empty = E 
    fun isEmpty E = true | isEmpty _ = false

    fun merge (h, E) = h
      | merge (E, h) = h
      | merge (h_1 as T(_,x,a_1,b_1), h_2 as T(_, y, a_2, b_2)) = 
        if Elem.leq(x, y) then makeT(x, a_1, merge(b_1, h_2))
        else makeT(y, a_2, merge(h_1, b_2))

    fun insert (x, h) = merge(T(1,x,E,E), h)

    fun findMin E = raise Empty  
      | findMin (T(_,x,a,b)) = x
    fun deleteMin E = raise Empty
      | deleteMin (T(_,x,a,b)) = merge(a,b)
end

structure Leftist_Heap = LeftistHeap (Ordered_Ints)

fun generateList [] acc = acc
  | generateList (x :: xs) acc = generateList xs (Leftist_Heap.insert(x, Leftist_Heap.empty) :: acc)

fun createNewLayer ([], acc) = acc
  | createNewLayer ((x :: (y :: xs)), acc) =    createNewLayer (xs, (Leftist_Heap.merge(x, y) :: acc))
  | createNewLayer ((x :: xs), acc) =           createNewLayer (xs, (Leftist_Heap.merge(x, Leftist_Heap.empty) :: acc))

fun fromList [] = Leftist_Heap.empty
  | fromList (l as (x :: xs)) =
  let
    val heapsOneElem = generateList l []
    fun generateUntilOne [] = Leftist_Heap.empty
      | generateUntilOne (x :: nil) = x
      | generateUntilOne (currentLayer as (x :: (y :: xs))) = generateUntilOne (createNewLayer (currentLayer, []))
  in
    generateUntilOne heapsOneElem
  end
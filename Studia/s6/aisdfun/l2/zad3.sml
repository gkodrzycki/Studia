
signature Ordered =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
  val toString : T -> string
end

signature Heap =
sig
  structure Elem : Ordered
  type Heap
  val empty : Heap
  val isEmpty : Heap -> bool
  val insert : Elem.T * Heap -> Heap
  val merge : Heap * Heap -> Heap
  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
  val printHeap : Heap -> unit
end

functor LeftistHeap (Element : Ordered) =
struct
  structure Elem = Element
  datatype Heap = E | T of int * Elem.T * Heap * Heap
  fun rank E = 0
    | rank (T(r,_,_,_)) = r
  fun makeT (x,a,b) = if rank a >= rank b then T (rank b+1,x,a,b) 
                      else T(rank a + 1,x,b,a)
  
  val empty = E
  fun isEmpty E = true 
    | isEmpty _ = false
  
  fun merge (h,E) = h
    | merge (E,h) = h
    | merge (h_1 as T(_,x,a_1,b_1), h_2 as T(_,y,a_2,b_2)) = 
      if Elem.leq (x,y) then makeT (x,a_1,merge (b_1,h_2)) 
      else makeT (y,a_2,merge (h_1,b_2))
  
  fun insert (x,h) = merge (T(1,x,E,E),h)
  
  fun findMin E = raise Empty
    | findMin (T(_,x,a,b)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T(_,x,a,b)) = merge(a,b)

  fun printHeap t =
   let
      fun spaces 0 = ""
        | spaces n = "   " ^ spaces (n-1)
      fun aux lvl E = print (spaces lvl ^ "E\n")
        | aux lvl (T(_, x, l, r)) =
          (
             aux (lvl+1) r;
             print (spaces lvl ^ Elem.toString(x) ^ "\n");
             aux (lvl+1) l
          )
   in
      aux 0 t
   end

end

structure Ordered_Ints : Ordered =
struct
  type T = int
  fun lt (a,b) = a < b
  fun eq (a,b) = a = b
  fun leq (a,b) = a <= b
  fun toString a = Int.toString a
end

structure Leftist_Heap = LeftistHeap (Ordered_Ints)

fun makeHeap ([],acc) = acc
  | makeHeap ((x::xs),acc) = makeHeap (xs,Leftist_Heap.insert (x,acc))

(* Teściki *)
(* val myList = [5, 3, 8, 2, 7]
val myHeap = makeHeap (myList, Leftist_Heap.empty)
val sortedList = list_of_heap (myHeap, []) *)
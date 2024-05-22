signature DEQUE =
sig
    type 'a Queue
    val empty : 'a Queue
    val isEmpty : 'a Queue -> bool
    
    val cons : 'a * 'a Queue -> 'a Queue
    val head : 'a Queue -> 'a
    val tail : 'a Queue -> 'a Queue

    val snoc : 'a Queue * 'a -> 'a Queue
    val last : 'a Queue -> 'a
    val init : 'a Queue -> 'a Queue
    
end

structure Deque = 
struct
  type 'a Queue = 'a list * 'a list * int

  val empty = ([], [], 0)

  fun isEmpty ([], [], x) = x = 0

  fun check (f, r, 0) = (f,r,0)
    | check (f, r, 1) = (f,r,1)
    | check ([], r, cnt) = 
    let val nr = List.length r  
        val left = List.take (r, nr div 2) in 
      (List.rev (List.drop (r, nr - (List.length left))), left, cnt)
    end
    | check (r, [], cnt) = 
    let val nr = List.length r 
        val left = List.take (r, (List.length r) div 2) in 
        (left, List.rev ( List.drop (r, nr - (List.length left))), cnt)
    end
    | check q = q

  fun cons (x, (f,r,cnt)) = check(x::f, r, cnt+1)

  fun head ([], [], 0) = raise Empty
    | head ([], x :: [], 1) = x
    | head (x :: f, r, _) = x

  fun tail (_, _, 0) = raise Empty 
    | tail (x::f, r, cnt) = check(f,r,cnt-1)

  fun snoc (x, (f, r, cnt)) = check (f, x::r, cnt+1)

  fun last ([], [], 0) = raise Empty
    | last (x :: [], [], 1) = x
    | last (f, x :: r, _) = x

   fun init (_, _, 0) = raise Empty 
    | init (f, x::r, cnt) = check(f,r,cnt-1)

end

val test1 = Deque.empty
val test2 = Deque.cons(1,test1)
val test3 = Deque.cons(5,test2)
val test4 = Deque.cons(6,test3)
val test5 = Deque.cons(8,test4)
val test6 = Deque.cons(10,test5)
val test7 = Deque.init test6
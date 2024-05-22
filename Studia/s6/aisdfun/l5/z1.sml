signature QUEUE =
sig
    type 'a Queue
    val empty : 'a Queue
    val isEmpty : 'a Queue -> bool
    
    val check : 'a Queue -> 'a Queue
    
    val head : 'a Queue -> 'a
    val tail : 'a Queue -> 'a Queue
    
    val snoc : 'a Queue * 'a -> 'a Queue
end

structure Queue = 
struct
  datatype 'a Queue = E | Q of 'a * 'a list * 'a list
  
  val empty = E
  
  fun isEmpty E = true
    | isEmpty _ = false

  (* fun isEmpty2 x = x = E *)
  
  fun tail E = raise Empty
    | tail (Q(x, [], [])) = E
    | tail (Q(_, [], r)) = Q(hd (rev r), tl(rev r), [])
    | tail (Q(_, x::f, r)) = Q(x, f, r)

  fun head E = raise Empty
    | head (Q(h, f, r)) = h
  
  (* fun tail E = raise Empty
    | tail q = checkf q *)
  
  fun snoc (E, x) = Q(x, [], [])
    | snoc (Q(h, f, r), x) =  Q(h, f, x :: r)

end
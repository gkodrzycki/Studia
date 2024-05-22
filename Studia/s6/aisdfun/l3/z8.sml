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

signature SET =
  sig
  structure Elem : ORDERED
  type Set
  val empty : Set
  val singleton : Elem.T -> Set
  val toList : Set -> Elem.T list
  val fromList : Elem.T list -> Set
  val fromOrdList : Elem.T list -> Set
  val size : Set -> int
  val member : Elem.T * Set -> bool
  val find : Elem.T * Set -> Elem.T option
  val add : Elem.T * Set -> Set
  val delete : Elem.T * Set -> Set
  val union : Set * Set -> Set
  val intersection : Set * Set -> Set
  val difference : Set * Set -> Set
end


functor UnbalancedTree(Element : ORDERED) : SET = 
struct
    structure Elem = Element
    datatype Tree = E | T of Tree * Elem.T * Tree
    type Set = Tree

    val empty = E
    fun singleton x = T(E,x,E)

    fun size E = 0
        | size (T(h1, x, h2)) = size h1 + size h2 + 1

    fun member (x, E) = false
        | member (x, T(a,y,b)) = 
            if Elem.lt (x, y) then member (x, a)
            else if Elem.lt (y, x) then member (x, b)
            else true

    fun find (x, h) = if member (x, h) then SOME x else NONE
    
    fun add (x, E) = singleton x
        | add (x, s as T(a,y,b)) = 
              if Elem.lt (x, y) then T(add (x,a), y, b)
              else if Elem.lt(y,x) then T(a, y, add (x,b))
              else s

    fun fromList [] = E
        | fromList (x::xs) = add (x, (fromList xs))

    fun toList E = []
        | toList (T(t1,x,t2)) = toList t2 @ (x::(toList t1)) 

    fun fromOrdList xs = fromList xs

    fun delete (x, E) = raise Empty
        | delete(x, h as T(h1, y, h2)) = 
            let 
                fun goRight (h as T(h1, x, E)) = (x, h1) 
                    | goRight (h as T(h1, y, h2)) = 
                    let 
                        val (maksLeft, newTree) = goRight h2 
                    in 
                        (maksLeft, T(h1, y, newTree))
                    end
                fun del_helper(x, h as T(h1, y, h2)) =
                    if Elem.leq (x, y) then
                        if Elem.leq (y, x) then 
                            let val (newElem, newH1) = goRight h1 in T(newH1, newElem, h2) end
                        else
                            T(del_helper(x, h1),y,h2)
                    else 
                        T(h1,y,del_helper(x,h2))
            in 
                if member (x, h) then 
                    del_helper(x, h) 
                else
                    h
            end

    fun union(x, y) = fromList(toList x @ toList y) 
    fun intersection (x, y) = foldl (fn (x, acc) => if member(x, y) then add(x, acc) 
                                                    else acc) empty (toList x)
    fun difference (h1, h2) = foldl (fn (x,acc) => delete(x, acc)) h1 (toList h2) 
    
end 

functor RedBlackSet (Element : ORDERED) : SET = 
struct
    structure Elem = Element
    datatype Color = R | B
    datatype Tree = E | T of Color * Tree * Elem.T * Tree
    type Set = Tree
    
    val empty = E
    fun singleton x = T(B,E,x,E)

    fun size E = 0
        | size (T(_,h1, x, h2)) = size h1 + size h2 + 1

    fun member (x, E) = false
        | member (x, T(_,a,y,b)) = 
            if Elem.lt (x, y) then member (x, a)
            else if Elem.lt (y, x) then member (x, b)
            else true
    
    fun find (x, h) = if member (x, h) then SOME x else NONE

    fun balance ((B,T (R, T(R,a,x,b),y,c),z,d)
        | (B,T (R,a,x,T(R,b,y,c)),z,d)
        | (B,a,x,T (R,T(R,b,y,c),z,d))
        | (B,a,x,T (R,b,y,T(R,c,z,d)))) = T(R, T(B,a,x,b),y,T(B,c,z,d))
        | balance body = T body
  

    fun add (x, s) =
        let fun ins E = T(R, E, x, E)
            | ins (s as T (color, a, y, b)) = 
                if Elem.lt (x, y) then balance (color, ins a, y, b)
                else if Elem.lt (y, x) then balance (color, a, y, ins b)
                else s
            val T(_,a,y,b) = ins s 
        in T(B,a,y,b) end

    fun fromList [] = E
        | fromList (x::xs) = add (x, (fromList xs))

    fun toList E = []
        | toList (T(_,t1,x,t2)) = toList t2 @ (x::(toList t1))
    
    fun floorLog2 n =
        let
            fun log2helper (x, acc) =
                if x <= 1 then acc
                else log2helper (x div 2, acc + 1)
        in
            log2helper (n, 0)
        end
      ;

    fun fromOrdList xs =
    let
        fun buildTreeFromSorted ([], _, _, _) = E
            | buildTreeFromSorted (x::xs, cur_len, cur_h, bh) =
                let
                val halfLength = cur_len div 2
                val (leftHalf, y::rightHalf) = List.splitAt (x::xs, halfLength)
                val leftTree = buildTreeFromSorted(leftHalf, halfLength, cur_h+1, bh)
                val rightTree = buildTreeFromSorted(rightHalf, cur_len - halfLength - 1, cur_h+1, bh)
                in
                if cur_h > bh then 
                    T(R,leftTree,y,rightTree)
                else
                    T(B,leftTree,y,rightTree)
                end
    in
        buildTreeFromSorted(xs, length xs, 1, floorLog2 (length xs))
    end
    ;

    fun value E = raise Empty 
        | value (T(_,_,x,_)) = x

    fun delete (x, E) = E
      | delete (x, T (color, a, y, b)) =
    if Elem.lt (x, y) then balance (color, delete (x, a), y, b)
    else if Elem.lt (y, x) then balance (color, a, y, delete (x, b))
    else
        let
            fun deleteMin (E, _) = raise Empty
              | deleteMin (T (_, E, y, E), min) = (y, min)
              | deleteMin (T (c, l, y, r), min) =
                let
                    val (leftMin, newLeft) = deleteMin (l, l)
                in
                    (leftMin, T (c, newLeft, y, r))
                end
            val (successor, newRight) = deleteMin (b, value b)
        in
            balance (color, a, successor, newRight)
        end
    
    fun union(x, y) = fromList(toList x @ toList y) 
    fun intersection (x, y) = foldl (fn (x, acc) => if member(x, y) then add(x, acc) 
                                                    else acc) empty (toList x)
    fun difference (h1, h2) = foldl (fn (x,acc) => delete(x, acc)) h1 (toList h2)
    
end

structure MyRB = RedBlackSet(IntOrdered)

val myRB = MyRB.empty;
val myRBWithElement1 = MyRB.add(2, myRB);
val myRBWithElement2 = MyRB.add(1, myRBWithElement1);
val myRBWithElement3 = MyRB.add(3, myRBWithElement2);
val myRBWithElement4 = MyRB.add(5, myRBWithElement3) 


(* functor AVLTree(Elem : ORDERED) =
struct
    structure Unbalanced = UnbalancedTree(Elem)

    open Unbalanced
    
    datatype Tree = E | T of int * Elem * Tree * Tree

     fun member (x, E) = false
        | member (x, T(_, y, a, b)) = 
            if Elem.lt (x, y) then member (x, a)
            else if Elem.lt (y, x) then member (x, b)
            else true

    fun height E = 0
      | height (T(h,_,_,_)) = h

    fun balance_factor E = 0
        | balance_factor (T(_,_,l,r)) = (height l) - (height r)

    fun max (a, b) = if a > b then a else b

    fun makeNode (v, l, r) = T(max ((height l), (height r)), v, l, r) 

    fun updateHeight E = E
      | updateHeight (T(_, x, left, right)) = T(1 + max ((height left), (height right)), x, left, right)

    fun rotateLeft (T(_, x, a, T(_, y, b, c))) = makeNode(y, makeNode(x, a, b), c) 

    fun rotateRight (T(_, x, T(_, y, a, b), c)) = makeNode(y, a, makeNode(x, b, c))

    fun balance (t as T(h, v, l, r)) =
        case ((balance_factor t), (balance_factor l), (balance_factor r)) of 
            (2, ~1, _) => rotateRight(makeNode(v, rotateLeft l, r))
            | ( 2, _, _)  => rotateRight(t)
            | (~2, _, 1)  => rotateLeft (makeNode(v, l, rotateRight r))
            | (~2, _, _)  => rotateLeft (t)
            | _ => t

    fun insert (x, E) = T(1, x, E, E)
      | insert (x, t as T(_, y, left, right)) =
        if Elem.lt (x, y) then balance (makeNode(y, insert (x, left), right))
        else if Elem.lt (y, x) then balance (makeNode(y, left, insert (x, right)))
        else t
end  *)



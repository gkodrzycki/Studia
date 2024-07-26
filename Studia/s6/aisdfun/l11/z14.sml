datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
datatype 'a Digit = One of 'a Tree | Two of 'a Tree * 'a Tree
type 'a RList = 'a Digit list

val empty = []
fun isEmpty ts = null ts


fun size (Leaf x) = 1
    | size (Node(w,t1,t2)) = w


fun link (t1,t2) = Node(size t1 + size t2, t1, t2)


fun consTree(t,[]) = [t]
    | consTree(t, One t1 :: ts) = Two(t, t1) :: ts
    | consTree(t, Two(t1,t2) :: ts) = One (t) :: consTree(link(t1,t2), ts) 


fun cons (x, ts) = consTree(Leaf x, ts)


fun head (One(Leaf x) :: _) = x
    | head (Two(Leaf x, Leaf y) :: _) = x


fun tail (One(Leaf x) :: ts) = ts
    | (Two(Leaf x, Leaf y) :: ts) = One(Leaf y) :: ts


fun lookupTree(0, Leaf x) = x
    | lookupTree(i, Leaf x) = raise Subscript
    | lookupTree(i, Node(w,t1,t2)) = 
        if i < w div 2 then lookupTree(i, t1) 
        else lookupTree(i - w div 2, t2)


fun updateTree(0, y, Leaf x) = Leaf y
    | updateTree(i, y, Leaf x) = raise Subscript
    | updateTree(i, y, Node(w,t1,t2)) = 
        if i < w div 2 then Node(w,updateTree(i, y, t1),t2) 
        else Node(w,t1,updateTree(i - w div 2, y, t2)) 


fun lookup (i, []) = raise Subscript
    | lookup (i, One t :: ts) = 
        if i < size t then lookupTree(i, t) else lookup(i - size t, ts)
    | lookup (i, Two (t1, t2) :: ts) = 
        if i <= size t1 then lookupTree(i, t1) 
        else if i <= size t2 + size t1 lookupTree(i - size t1, t2) 
        else lookup(i - size t1 - size t2, ts)
    
        
fun update (i, y, []) = raise Subscript
    | update(i, y, One t :: ts) =
        if i < size t then One (updateTree(i,y,t)) :: ts
        else One t :: update(i - size t, y, ts)
    | update(i, y, Two(t1,t2) :: ts) = 
        if (i < size t1) then Two(updateTree(i, y, t1), t2) :: ts
        else if (i < size t1 + size t2) then Two(t1, updateTree(i - size t1,y,t2)) :: ts
        else Two(t1,t2) :: update(i - size t1 - size t2, y, ts)

        
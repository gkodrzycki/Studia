structure BinaryRandomAccessList = 
struct
    datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
    type 'a RList = 'a Tree list

    val empty = []
    fun isEmpty ts = null ts

    fun size (Leaf x) = 1
        | size (Node(w,t1,t2)) = w

    fun link (t1,t2) = Node(size t1 + size t2, t1, t2)

    fun consTree(t,[]) = [t]
        | consTree(t, tss as (t1 :: ts)) = 
            if size t < size t1 then t :: tss
            else consTree(link(t,t1), ts)


    fun getLeaf (Leaf x, acc) = (x, acc)
        | getLeaf (Node(_, t1, t2), acc) = getLeaf(t1, t2::acc)

    fun unconsTree [] = raise Empty
        | unconsTree(x :: xs) = getLeaf(x, xs)


    fun cons (x, ts) = consTree(Leaf x, ts)
    fun head ts = let val (Leaf x, _) = unconsTree ts in x end
    fun tail ts = let val (_, ts') = unconsTree ts in ts' end

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
        | lookup (i, t :: ts) = 
            if i < size t then lookupTree (i,t) else lookup(i - size t, ts)
            
    fun update (i, y, []) = raise Subscript
        | update (i, y, t :: ts) = 
            if i < size t then updateTree (i,y,t) :: ts 
            else t :: update(i - size t, y, ts)
end
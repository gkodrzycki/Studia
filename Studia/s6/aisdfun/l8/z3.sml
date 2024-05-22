functor SizedHeap (H : HEAP) : HEAP =
struct
    structure Elem = H.Elem
    datatype Heap = Int * H.Heap
    
    val empty = (0, E)
    
    fun isEmpty (0, _) = true
        | isEmpty _ = false
        
    fun insert (x, (n1,h)) = (n1 + 1, H.insert(x,h))
    
    fun merge ((n1,h1), (n2,h2)) = (n1 + n2, H.merge(h1,h2))
    
    fun deleteMin (n, h) = (n - 1, H.deletemin(h))
    
    fun findMin (n, h) = H.findMin(h)
end
    
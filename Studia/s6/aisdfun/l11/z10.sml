fun drop(_, []) = raise Subscript
    | drop(0, ts) = ts
    | drop(k, Zero t :: ts) = drop(k, ts)
    | drop(k, One t :: ts) = 
        if k >= size t then drop(k - size t, ts)
        else dropTree(k, t, ts) 

fun dropTree(0, ts, acc) = ts
    | dropTree(k, Node(w, t1, t2), acc) =
        if k <= w div 2 then dropTree(k, t1, t2 :: acc)
        else dropTree(k - w div 2, t2, acc)
fun create(0, label, _)   = []
    | create(n, label, s) = 
        if n mod 2 = 1 then One bt(label, s) :: create(n - s, label, 2*s)
        else Zero :: create(n, label, 2*s)
    
fun bt(label, 1) = Leaf label
    | bt(label, n) = 
        let 
            val t = bt(label, (n-1) div 2)
        in
            Node(t, t)
        end
(* Zadanie 7 *)

type 'a lheap = 
    | Leaf
    | Node of 'a lheap * 'a * int * 'a lheap ;;

let make label aheap bheap = 
    match aheap, bheap with
    | Leaf, Leaf -> Node(Leaf, label, 1, Leaf)
    | Leaf, node
    | node, Leaf -> Node(node,label,1,Leaf)
    | Node(_,_,arank,_), Node(_,_,brank,_) ->
            if arank >= brank
            then Node(aheap,label,brank + 1,bheap) 
            else Node(bheap,label,arank + 1,aheap) ;;

let rec merge aheap bheap =
    match aheap, bheap with
    | Leaf, Leaf -> Leaf
    | Leaf, node | node, Leaf -> node
    | Node(aleft,alabel,_,aright), Node(bleft,blabel,_,bright) ->
            if alabel >= blabel
            then make blabel bleft (merge bright aheap)
            else make alabel aleft (merge aright bheap)
;;

let add elem heap =
    merge (make elem Leaf Leaf) heap ;;

let del heap =
    match heap with
    | Leaf -> Leaf
    | Node(l,_,_,r) -> merge l r ;;

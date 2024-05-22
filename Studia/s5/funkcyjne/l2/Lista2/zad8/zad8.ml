type 'a leftistTree = 
  | Leaf
  | Node of 'a leftistTree * 'a * int * 'a leftistTree

let empty = Leaf

let rightHeight x = match x with
| Leaf -> 0
| Node(_,_,x,_) -> x

let create subtree1 subtree2 root = 
  match subtree1, subtree2 with
  | Leaf, Leaf -> Node(Leaf, root, 1, Leaf)
  | Leaf, Node(l,x,h,r) -> Node(subtree2, root, h+1, Leaf)
  | Node(l,x,h,r), Leaf -> Node(subtree1, root,  1, Leaf)
  | Node(l1,x1,h1,r1), Node(l2,x2,h2,r2) 
  -> if h2 < h1 then 
    Node(subtree1, root, h1+1, subtree2)
  else  
    Node(subtree2, root, h2+1, subtree1)

let rec merge tree1 tree2 = match tree1,tree2 with
| Leaf, Leaf -> Leaf
| Node(l,x,h,r), Leaf -> tree1
| Leaf, Node(l,x,h,r) -> tree2
| Node(l1,x1,h1,r1), Node(l2,x2,h2,r2) -> if(x1 <= x2) 
  then let newTree = merge r1 tree2 in if (rightHeight newTree) >= (rightHeight l1) then Node(newTree,x1,(rightHeight l1) + 1,l1) else Node(l1,x1,(rightHeight newTree) + 1,newTree)
  else let newTree = merge r2 tree1 in if (rightHeight newTree) >= (rightHeight l2) then Node(newTree,x2,(rightHeight l2) + 1,l2) else Node(l2,x2,(rightHeight newTree) + 1,newTree)


let add_elem x tree = match tree with
| Leaf -> create Leaf Leaf x
| tree -> merge (create Leaf Leaf x) tree 


let del_min tree = match tree with
| Leaf -> None,empty
| Node(l,x,h,r) -> Some x, merge l r 

type 'a priority_queue = {q : 'a leftistTree}

let pq_add x queue = {q = add_elem x queue.q}

let pq_min queue = fst (del_min queue.q)

let pq_rm queue = {q = snd (del_min queue.q) }

let pq_create = {q = empty}

let pq_merge q1 q2 = {q = merge q1.q q2.q}


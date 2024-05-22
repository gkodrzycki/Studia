datatype tree = & of int * (tree * tree) | %
infix &
fun member (x,y&(t1,t2)) =
        if x <= y
            then if y <= x
                then true
                else member (x,t1)
        else member (x,t2)
    | member (x,%) = false
;

fun insert (x, t as y&(t1,t2)) =
        if x <= y
            then if y <= x
                then t
                else y&(insert (x,t1), t2)
        else y&(t1, insert (x,t2))
    | insert (x,%) = x&(%,%)
;
(* -------------------------------------------------------- *)

fun lower (x,y&(t1,t2), v) =
        if y >= x
            then lower (x,t1,y)
            else lower (x,t2,v)
    | lower (x,%,v) = v
;
fun root (x&(t1,t2)) = 
    x;
;
fun memberh1 (x, t) = 
    x >= lower(x, t, root(t))
;


fun helper (x, t as y&(t1,t2)) =
        if y > x
            then y&(helper (x,t1), t2)
            else y&(t1, helper (x,t2))
    | helper (x,%) = x&(%,%)
;
fun insert (x, t) = 
    if memberh1(x, t) 
        then t
        else helper(x, t)
;


fun inserttop (x, y&(t1,t2), v, org, cps) = 
    if y >= x
            then inserttop (x,t1,y, org, fn e => cps(y&(e,t2)))
            else inserttop (x,t2,v, org, fn e => cps(y&(t1,e)))
    | inserttop (x,%,v,org,cps) = 
        if v <= x 
            then org
            else cps(x&(%,%))
;
(* Testy  *)
val tree = (5&(4&(%,%),7&(6&(%,%),9&(%,%))));

val tree2 = insert(8, tree)
val tree2t = inserttop(8, tree, root(tree), tree, fn e => e)


val tree3 = insert(7, tree) 
val tree3t = inserttop(7, tree, root(tree), tree, fn e => e)

val test7 = member(7, tree);
val test8 = member(8, tree);

val test7h = memberh1(7, tree);
val test8h = memberh1(8, tree);

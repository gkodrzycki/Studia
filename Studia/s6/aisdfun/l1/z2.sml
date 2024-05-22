fun quicksort cmp (x::xs) = 
    let
        fun split (small,big) (y::ys) = 
        if cmp y x 
            then split (y :: small, big) ys
            else split (small, y :: big) ys
        | split (small, big) nil = quicksort cmp small @ [x] @ quicksort cmp big
    in 
        split (nil,nil) xs
    end
    | quicksort cmp [] = [] 

fun less a b = a < b;
fun more a b = a > b;
fun lesseq a b = a <= b;

val test = [5,2,6,10,11,1,4,4]

val sortedtest1 = quicksort less test
val sortedtest2  = quicksort more test
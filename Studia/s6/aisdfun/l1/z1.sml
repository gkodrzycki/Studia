fun sublist (x::xs) =
    let
        fun addx (ys::yss) = (x::ys) :: addx yss
            | addx nil = nil    
        val xss = sublist xs
    in
        xss @ addx xss
    end
    | sublist [] = [[]]


(* Przypał z nieużytakmi w czasie @, bo za każdym razem kopiujemy te wszystkie poprzednie wartości, na oko jest ich sum from i=0 to n-1 2^i  *)

fun bsublist (x::xs) =
    let
        fun addx (ys::yss) =  (x::ys) :: ys :: addx yss
            | addx nil = nil    
        val xss = bsublist xs
    in
        addx xss
    end
    | bsublist [] = [[]]

val s  =  sublist([1,2,3]);
val ss = bsublist([1,2,3]);

(* --------------------------------------------------------------- *)
datatype tree = & of int * (tree * tree) | %
infix &
fun flatten (x&(t1,t2)) = flatten t1 @ [x] @ flatten t2
    | flatten % = nil

(* To jakieś harde ale chyba po prostu suma po liczbie lewych poddrzew? *)

fun bflatten (x&(t1,t2)) = 
    let
        fun helper (x&(t1,t2)) acc = helper t1 (x :: helper t2 acc)
            | helper % acc = acc
    in
        helper (x&(t1,t2)) []
    end
    | bflatten % = []


val tree = (5&(3&(%,4&(%,%)),10&(%,%)));
val t = flatten(tree);
val tt = bflatten(tree);
(* -------------------------------------------------------------- *)
fun rev (x::xs) = rev xs @ [x]
    | rev nil = nil

(* Przypał z nieużytakmi w czasie @, bo za każdym razem kopiujemy te wszystkie poprzednie wartości, na oko jest ich Sn-1  *)

(* better rev *)
fun rev_helper([], acc) = acc
  | rev_helper(x::xs, acc) = rev_helper(xs, x::acc);

fun brev(xs) = rev_helper(xs, []);

fun brev2([]) = []
  | brev2(x::xs) =
    let
      fun brev_helper([], acc) = acc
        | brev_helper(x::xs, acc) = brev_helper(xs, x::acc)
    in
      brev_helper(x::xs, [])
    end;

val rr = rev([1,2,3]);
val r  = brev([1,2,3]);  
val rrr = brev2([1,2,3]);
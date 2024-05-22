fun merge cmp ([], []) = []
  | merge cmp (x,[]) = x
  | merge cmp ([],y) = y
  | merge cmp (x :: xs, y :: ys) = 
    if cmp x y then x :: merge cmp (xs, (y::ys))
    else y :: merge cmp ((x::xs), ys)

fun generateList [] acc = acc
  | generateList (x :: xs) acc = generateList xs ([x] :: acc)

fun createNewLayer cmp ([], acc) = acc
  | createNewLayer cmp ((x :: (y :: xs)), acc) =    createNewLayer cmp (xs, merge cmp (x,y) :: acc)
  | createNewLayer cmp ((x :: xs), acc) =           createNewLayer cmp (xs, (x :: acc))

fun mergesort (cmp, []) = []
  | mergesort (cmp, (l as (x :: xs))) =
  let
    val heapsOneElem = generateList l []
    fun generateUntilOne [] = []
      | generateUntilOne (x :: nil) = x
      | generateUntilOne (currentLayer as (x :: (y :: xs))) = generateUntilOne (createNewLayer cmp (currentLayer, []))
  in
    generateUntilOne heapsOneElem
  end

fun le a b  = a < b

mergesort (le, [5,10,1,23,6,7,4]);

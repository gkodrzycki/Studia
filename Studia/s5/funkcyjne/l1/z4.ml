(* Definicje funkcji *)
let hd stream = stream(0)
let tl stream = fun x -> stream(x+1)

let add const stream = fun y -> const + stream(y)

let map func stream = fun y -> func(stream(y))

let map2 stream1 stream2 func = fun y -> func (stream1(y)) (stream2(y))

let replace n a stream = fun y -> if y == n then a else stream(y)

let take_every n stream = fun y -> stream(n*y)

let rec scan funkcja a stream = fun y -> if y == 0 then funkcja(a)(stream(0)) else funkcja((scan funkcja a stream) (y-1))(stream(y))  


(* Test *)

let rec stream_example x = if x == 0 then 0 else stream_example (x - 1) + 1 (* ciąg 0,1,2,3 *)

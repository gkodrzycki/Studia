let head stream = stream(0)
let tail stream = fun x -> stream(x+1)

let add const stream = fun y -> const + stream(y)

let map funkcja stream = fun y -> funkcja(stream(y))

let map2 stream1 stream2 funkcja = fun y -> funkcja (stream1(y)) (stream2(y))

let replace n a stream = fun y -> if y == n then a else stream(y)

let take_every n stream = fun y -> stream(n*y)



let rec stream_example x = if x == 0 then 0 else stream_example (x - 1) + 1 (* ciąg 0,1,2,3 *)


let test_stream = assert(stream_example 5 == 5) (* 5 elem to 5 *)

let test_head = assert(head stream_example == 0) (* początek to 0 *)

let test_tail = assert(head (tail stream_example) == 1) (* ogon początku to wsm 1 *)

let stream_add = add 10 stream_example
let test_add = assert(stream_add 5 == 15) 


let fun_example x = x*100
let stream_map = map fun_example stream_example
let test_map = assert(stream_map 5 == 500)

let stream_map2 = map2 stream_example stream_example (+)
let test_map2 = assert(stream_map2 5 == 10)

let stream_replace = replace 5 68 stream_example
let test_replace = assert(stream_replace 5 == 68)

let stream_take = take_every 5 stream_example
let test_take = assert(stream_take 5 == 25)



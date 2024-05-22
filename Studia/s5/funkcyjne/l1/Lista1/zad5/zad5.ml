let head stream = stream(0)
let tail stream = fun x -> stream(x+1)

let add const stream = fun y -> const + stream(y)

let map funkcja stream = fun y -> funkcja(stream(y))

let map2 stream1 stream2 funkcja = fun y -> funkcja(stream1(y))(stream2(y))

let replace n a stream = fun y -> if y == n then a else stream(y)

let take_every n stream = fun y -> stream(n*y)

let rec scan funkcja a stream = fun y -> if y == 0 then funkcja(a)(stream(0)) else funkcja((scan funkcja a stream) (y-1))(stream(y))  


let rec tabulate ?(l = 0) r stream = if r == l then [] else  (stream l) :: (tabulate ~l:(l+1) r stream)


let rec stream_example x = if x == 0 then 0 else stream_example (x - 1) + 1 (* ciąg 0,1,2,3 *)


let wow = tabulate ~l:0 5 stream_example
module type QUEUE =
sig
	type 'a queue

	val empty 	: 'a queue
	val isEmpty	: 'a queue -> bool
	val snoc	: 'a queue -> 'a -> 'a queue
	val head	: 'a queue -> 'a 
	val tail	: 'a queue -> 'a queue
end;;

module HoodMelvilleQueue : QUEUE = 
struct
	type 'a rotationState = 
		  IDLE
		| REV  of int * 'a list * 'a list * 'a list * 'a list
		| APP  of int * 'a list * 'a list
		| DONE of 'a list

	type 'a queue = int * 'a list * 'a rotationState * int * 'a list
	
	let exec rs = 
		match rs with
			| REV(ok, x::f, ff, y::r, rr) ->
				REV(ok+1, f, x::ff, r, y::rr)
			| REV(ok, [], ff, [y], rr) ->
				APP(ok, ff, y::rr)
			| APP(0, ff, rr) ->
				DONE(rr)
			| APP(ok, x::ff, rr) ->
				APP(ok-1, ff, x::rr)
			| state -> state

	let invalidate rs = 
		match rs with
			| REV(ok, f, ff, r, rr) ->
				REV(ok-1, f, ff, r, rr)
			| APP(0, ff, x::rr) ->
				DONE(rr)
			| APP(ok, ff, rr) -> 
				APP(ok-1, ff, rr)
			| state -> state
			
	let exec2 q = match q with 
		| (lenf, f, state, lenr, r) ->
			match (exec state) with
				| DONE(newf) ->
					(lenf, newf, IDLE, lenr, r)
				| newstate ->
					(lenf, f, newstate, lenr, r)

	let check q = match q with
		| (lenf, f, state, lenr, r) -> 
			if lenr <= lenf then exec2 q
			else
				let newstate = REV(0, f, [], r, [])
				in	exec2(exec2(lenf+lenr, f, newstate, 0, []))

	let empty = (0, [], IDLE, 0, [])

	let isEmpty q = match q with 
		| (lenf, f, state, lenr, r) ->
			(lenf == 0)

	let snoc q x = match q with 
		| (lenf, f, state, lenr, r) -> check (lenf, f, state, lenr+1, x::r)

	let head q = match q with 
		| (lenf, [], state, lenr, r) -> failwith "Empty"
		| (lenf, x::f, state, lenr, r) -> x

	let tail q = match q with 
		| (lenf, [], state, lenr, r) -> failwith "Empty"
		| (lenf, x::f, state, lenr, r) -> 
			check(lenf-1, f, invalidate state, lenr, r)
		
end;;


module Q = HoodMelvilleQueue;;

let xd = Q.empty;;

let xd = Q.snoc xd 1;;
let xd = Q.snoc xd 2;;
let xd = Q.snoc xd 3;;
let xd = Q.snoc xd 4;;
let xd = Q.snoc xd 5;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

let xd = Q.snoc xd 1;;
let xd = Q.snoc xd 2;;
let xd = Q.snoc xd 3;;
let xd = Q.snoc xd 4;;
let xd = Q.snoc xd 5;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

Printf.printf "%d\n" (Q.head xd);;
let xd = Q.tail xd;;

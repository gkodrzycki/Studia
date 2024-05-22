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

	type 'a queue = int * 'a list * 'a rotationState * 'a list
	
	let exec rs = 
		match rs with
			| REV(ok, x::f, ff, y::r, rr) ->
				(1, REV(ok+1, f, x::ff, r, y::rr))
			| REV(ok, [], ff, [y], rr) ->
				(1, APP(ok, ff, y::rr))
			| APP(0, ff, rr) ->
				(0, DONE(rr))
			| APP(ok, x::ff, rr) ->
				(1, APP(ok-1, ff, x::rr))
			| state -> (0, state)

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
		| (diff, f, state, r) ->
			match (exec state) with
				| (0, DONE(newf)) ->
					(diff, newf, IDLE, r)
				| (d, newstate) ->
					(diff + d, f, newstate, r)

	let check q = match q with
		| (diff, f, state, r) -> 
			if diff>=0 then exec2 q
			else
				let newstate = REV(1, f, [], r, [])
				in	exec2(exec2(-1, f, newstate, []))

	let empty = (0, [], IDLE, [])

	let isEmpty q = match q with 
		| (diff, [], state, r) -> true
		| _ -> false

	let snoc q x = match q with 
		| (diff, f, state, r) ->
			check (diff-1, f, state, x::r)

	let head q = match q with 
		| (diff, [], state, r) -> failwith "Empty"
		| (diff, x::f, state, r) -> x

	let tail q = match q with 
		| (diff, [], state, r) -> failwith "Empty"
		| (diff, x::f, state, r) -> 
			check(diff-1, f, invalidate state, r)
		
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

let print_list f lst =
    let rec print_elements = function
        | [] -> ()
        | h::t -> f h; print_string ";"; print_elements t
    in
    print_string "[";
    print_elements lst;
    print_string "]";;

module type ORDERED = 
sig
    type t
    val eq    : t -> t -> bool
    val le    : t -> t -> bool
    val leq   : t -> t -> bool
end;;

module OrderedInt =
struct
    type t = int
    let eq a b = (a=b)
    let le a b = (a<b)
    let leq a b = (a<=b)
end;;

module RedBlackTree (Elem : ORDERED) =
struct


    type color = R | B
    type tree  = E | T of bool * color * tree * Elem.t * tree
	type root  = RR of tree * int * int
	
    let empty = RR(E, 0, 0)

	let rec pure_member x t = 
		match t with
			| E -> false
			| T(by, _, l, y, r) ->
				if (Elem.eq x y) then by
				else if Elem.le x y then (pure_member x l)
				else (pure_member x r)

	let balance u = 
		match u with 
            | T(bz, B, T(by, R, T(bx, R, a, x, b), y, c), z, d)	-> T(by, R, T(bx, B, a, x, b), y, T(bz, B, c, z, d))
            | T(bz, B, T(bx, R, a, x, T(by, R, b, y, c)), z, d)	-> T(by, R, T(bx, B, a, x, b), y, T(bz, B, c, z, d))
            | T(bx, B, a, x, T(bz, R, T(by, R, b, y, c), z, d))	-> T(by, R, T(bx, B, a, x, b), y, T(bz, B, c, z, d))
            | T(bx, B, a, x, T(by, R, b, y, T(bz, R, c, z, d)))	-> T(by, R, T(bx, B, a, x, b), y, T(bz, B, c, z, d))
            | T(bx, x, d, xd, dx) -> T(bx, x, d, xd, dx)
			| E -> E
	
	let rec pure_insert x t = 
		let rec ins t = 
			match t with 
				| E -> (T(true, R, E, x, E), 1)
				| T(by, color, a, y, b) ->
                    if (Elem.le x y) then 
                    	let (aa, res) = ins a in ((balance (T(by, color, aa, y, b))), res)
                    else if (Elem.le y x) then 
                    	let (bb, res) = ins b in ((balance (T(by, color, a, y, bb))), res)
                    else (T(true, color, a, y, b), if by then 0 else 1)
		in 
            let tmp = ins t
			in match tmp with 
                | (T(bx, _, a, x, b), res) -> (T(bx, B, a, x, b), res)
				| (E, res) -> (E, res)

	let rec pure_delete x t = 
		match t with
			| T(by, color, a, y, b) ->
                if (Elem.le x y) then
                	let (aa, res) = pure_delete x a in ((balance (T(by, color, aa, y, b))), res)
                else if (Elem.le y x) then 
                	let (bb, res) = pure_delete x b in ((balance (T(by, color, a, y, bb))), res)
                else (T(false, color, a, y, b), if by then 1 else 0)
			| E -> (E, 0)

	let member x root =
		match root with
			| RR(t, alive, size) -> pure_member x t

	let insert x root =
		match root with
			| RR(t, alive, size) -> 
				let (tt, res) = pure_insert x t
				in RR(tt, alive+res, size+res)

	let rec to_list t acc =
		match t with
			| T(alive, color, l, x, r) ->
				let res = to_list l acc
				in to_list r (if alive then x::res else res)
			| E -> acc


	let rec from_ord_list xs =
		let rec mktree n k xs =
			if (n = 0) then (E, xs)
			else
				match xs with 
					| z::zzs -> 
						if (n=1) then 
							(T(true, (if k > 1 then B else R), E, z, E), zzs)
						else
							let n2 = (n-1)/2
							in let k2 = k/2
							in let (l, y::ys) = mktree n2 k2 xs
							in let (r, yss)   = mktree (n-(1+n2)) k2 ys
							in (T(true, B, l, y, r), yss)
		in let n = List.length xs
		in let (t, acc) = mktree n (n+1) xs
		in RR(t, n, n)
			
	let rebuild root = 
		match root with
			| RR(t, alive, size) ->
				if (alive < (size/2)) then
					from_ord_list (List.rev (to_list t []))
				else 
					root

	let delete x root =
		match root with
			| RR(t, alive, size) -> 
				let (tt, res) = pure_delete x t
				in rebuild (RR(tt, alive-res, size))

	let tree r =
		match r with
			| RR(t, alive, size) -> t

	let alive r =
		match r with
			| RR(t, alive, size) -> alive

	let size r =
		match r with
			| RR(t, alive, size) -> size
end;;

module RGBT = RedBlackTree(OrderedInt);;

let xd = RGBT.empty;;

let xd = RGBT.insert 2 xd;;
let xd = RGBT.insert 3 xd;;
let xd = RGBT.insert 4 xd;;
let xd = RGBT.insert 5 xd;;
let xd = RGBT.insert 6 xd;;
let xd = RGBT.insert 7 xd;;
let xd = RGBT.insert 7 xd;;

Printf.printf "%b\n" (RGBT.member 0 xd);;
Printf.printf "%b\n" (RGBT.member 1 xd);;
Printf.printf "%b\n" (RGBT.member 2 xd);;
Printf.printf "%b\n" (RGBT.member 3 xd);;
Printf.printf "%b\n" (RGBT.member 4 xd);;
Printf.printf "%b\n" (RGBT.member 5 xd);;
Printf.printf "%b\n" (RGBT.member 6 xd);;
Printf.printf "%b\n" (RGBT.member 7 xd);;
Printf.printf "%b\n" (RGBT.member 8 xd);;
Printf.printf "%d/%d\n" (RGBT.alive xd) (RGBT.size xd);;
Printf.printf "\n";;

let xd = RGBT.delete 3 xd;;
Printf.printf "%d/%d\n" (RGBT.alive xd) (RGBT.size xd);;
let xd = RGBT.delete 4 xd;;
Printf.printf "%d/%d\n" (RGBT.alive xd) (RGBT.size xd);;
let xd = RGBT.delete 5 xd;;
Printf.printf "%d/%d\n" (RGBT.alive xd) (RGBT.size xd);;
let xd = RGBT.delete 6 xd;;
Printf.printf "%d/%d\n" (RGBT.alive xd) (RGBT.size xd);;

Printf.printf "%b\n" (RGBT.member 0 xd);;
Printf.printf "%b\n" (RGBT.member 1 xd);;
Printf.printf "%b\n" (RGBT.member 2 xd);;
Printf.printf "%b\n" (RGBT.member 3 xd);;
Printf.printf "%b\n" (RGBT.member 4 xd);;
Printf.printf "%b\n" (RGBT.member 5 xd);;
Printf.printf "%b\n" (RGBT.member 6 xd);;
Printf.printf "%b\n" (RGBT.member 7 xd);;
Printf.printf "%b\n" (RGBT.member 8 xd);;
Printf.printf "\n";;

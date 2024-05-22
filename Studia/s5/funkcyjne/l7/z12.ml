module type RandomMonad = sig
type 'a t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val random : int t
end

module RS : sig
include RandomMonad

val run : int -> 'a t -> 'a
end = struct
type 'a t = int -> 'a * int

let return a s = (a, s)

let bind m f s =
    let a, s = m s in
    f a s

let random s =
    let r = (16807 * (s mod 127773)) - (2836 * (s / 127773)) in
    let new_s = if r > 0 then r else r + 2147483647 in
    return r new_s

let run s m = fst (m s)
end

module Shuffle : sig
    val shuffle : 'a list -> 'a list RS.t
end = struct
    let ( let* ) = RS.bind
    let replace lst idx el = List.mapi (fun i a -> if i == idx then el else a) lst

    let shuffle lst =
        let rec _shuffle lst acc max_i =
        match lst with
        | [] -> RS.return acc
        | x :: [] -> RS.return (x :: acc)
        | x :: xs ->
            let* r = RS.random in
            let idx = (abs r) mod (max_i - 1) in
            let el = List.nth xs idx in
            _shuffle (replace xs idx x) (el :: acc) (max_i - 1)
        in _shuffle lst [] (List.length lst)
end

(*TESCIOR*)
RS.run 5 (Shuffle.shuffle [1;2;3;4;5]) ;;
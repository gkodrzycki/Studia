module My_lazy : sig
  type 'a t

  val force : 'a t -> 'a
  val delay : (unit -> 'a) -> 'a t
end = struct
  type 'a mylazy_t = Lazy of (unit -> 'a) | Done of 'a | InProgress
  type 'a t = 'a mylazy_t ref

  let force x =
    match !x with
    | Done a -> a
    | InProgress -> failwith "oblicza sie"
    | Lazy f ->
        x := InProgress;
        let a = f () in
        x := Done a;
        a

  let delay f = ref (Lazy f)
end

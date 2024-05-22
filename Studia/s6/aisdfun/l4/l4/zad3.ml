module Stream : sig
  type 'a streamCell = Cons of 'a * 'a stream | Nil
  and 'a stream = 'a streamCell Lazy.t

  val shd : 'a stream -> 'a
  val stl : 'a stream -> 'a stream
  val ( ++ ) : 'a stream -> 'a stream -> 'a stream
  val constStream : 'a -> 'a stream
  val mkStream : (int -> 'a) -> 'a stream
  val from : int -> int stream
  val sMap : ('a -> 'b) -> 'a stream -> 'b stream
  val sDrop : int -> 'a stream -> 'a stream
  val sTake : int -> 'a stream -> 'a list
  val zip : 'a stream -> 'b stream -> ('a * 'b) stream
  val unzip : ('a * 'b) stream -> 'a stream * 'b stream
  val splice : 'a stream -> 'b stream -> ('a * 'b) stream
  val filter : ('a -> 'b option) -> 'a stream -> 'b stream
  val find : ('a -> bool) -> 'a stream -> 'a stream
  val remove : ('a -> bool) -> 'a stream -> 'a stream
  val findOne : ('a -> bool) -> 'a stream -> 'a option
  val flatten : 'a stream stream -> 'a stream
  val nth : int -> 'a stream -> 'a
  val prefix : ('a -> bool) -> 'a stream -> 'a stream
  val suffix : ('a -> bool) -> 'a stream -> 'a stream
  val split : int -> 'a stream -> 'a stream * 'a stream
  val splitp : ('a -> bool) -> 'a stream -> 'a stream * 'a stream
  val foldl : ('a -> 'b -> 'a) -> 'b stream -> 'a -> 'a
  val dropOne : ('a -> bool) -> 'a stream -> 'a stream
  val sRev : 'a stream -> 'a stream
  val sRevMap : ('a -> 'b) -> 'a stream -> 'b stream
end = struct
  type 'a streamCell = Cons of 'a * 'a stream | Nil
  and 'a stream = 'a streamCell Lazy.t

  let rec foldl f s ini =
    match s with
    | (lazy (Cons (e, s))) -> foldl f s (f ini e)
    | (lazy Nil) -> ini

    let sRevMap f s =
        foldl (fun acc e -> lazy (Cons((f e),acc))) s (lazy Nil)

  let rec dropOne f s =
    match s with
    | (lazy (Cons (e, s))) when f e -> s
    | (lazy (Cons (e, s))) -> lazy (Cons (e, dropOne f s))
    | _ -> lazy Nil

  let shd s =
    match s with (lazy (Cons (a, _))) -> a | (lazy Nil) -> failwith "shd"

  let stl s =
    match s with (lazy (Cons (_, e))) -> e | (lazy Nil) -> failwith "stl"

  let rec ( ++ ) sx sy =
    match sx with
    | (lazy (Cons (e, sx))) -> lazy (Cons (e, sx ++ sy))
    | (lazy Nil) -> sy

  let rec constStream a = lazy (Cons (a, constStream a))
  let rec from n = lazy (Cons (n, from (n + 1)))

  let rec sMap f s =
    match s with
    | (lazy (Cons (e, s))) -> lazy (Cons (f e, sMap f s))
    | (lazy Nil) -> lazy Nil

  let mkStream f = from 0 |> sMap f

  let rec sDrop n s =
    match s with
    | (lazy (Cons (e, s))) when n > 0 -> sDrop (n - 1) s
    | (lazy s) -> lazy s

  let rec sTake n s =
    match s with
    | (lazy (Cons (e, s))) when n > 0 -> e :: sTake (n - 1) s
    | (lazy _) -> []

  let rec zip sa sb =
    match (sa, sb) with
    | (lazy (Cons (a, sa))), (lazy (Cons (b, sb))) ->
        lazy (Cons ((a, b), zip sa sb))
    | (lazy Nil), _ | _, (lazy Nil) -> lazy Nil

  let rec unzip sab =
    match sab with
    | (lazy (Cons ((a, b), sab))) ->
        let s = lazy (unzip sab) in
        ( lazy (Cons (a, fst (Lazy.force s))),
          lazy (Cons (b, snd (Lazy.force s))) )
    | (lazy Nil) -> (lazy Nil, lazy Nil)

  let rec splice sa sb =
    match sa with
    | (lazy (Cons (e, sa))) -> zip (constStream e) sb ++ splice sa sb
    | (lazy Nil) -> lazy Nil

  let filter f s =
    let rec _filter s =
      match s with
      | (lazy (Cons (Some e, s))) -> lazy (Cons (e, _filter s))
      | (lazy (Cons (_, s))) -> _filter s
      | (lazy Nil) -> lazy Nil
    in
    _filter (sMap f s)

  let find f s = filter (fun a -> if f a then Some a else None) s
  let remove f s = find (fun a -> f a |> not) s

  let findOne f s =
    match find f s with (lazy (Cons (e, _))) -> Some e | _ -> None

  let rec flatten ss =
    match ss with (lazy (Cons (s, ss))) -> s ++ flatten ss | _ -> lazy Nil

  let nth n s = sDrop n s |> shd

  let rec prefix f s =
    match s with
    | (lazy (Cons (e, s))) when f e -> lazy (Cons (e, prefix f s))
    | _ -> lazy Nil

  let rec suffix f s =
    match s with
    | (lazy (Cons (e, sa))) when f e -> s
    | (lazy (Cons (e, s))) -> suffix f s
    | (lazy Nil) -> lazy Nil

  let sRev s =
    let rec _rev acc s =
      match s with
      | (lazy (Cons (e, s))) -> _rev (lazy (Cons (e, acc))) s
      | (lazy Nil) -> acc
    in
    _rev (lazy Nil) s

  let rec split n s =
    let rec _split acc n s =
      match s with
      | (lazy (Cons (e, s))) when n > 0 ->
          _split (lazy (Cons (e, acc))) (n - 1) s
      | s -> (sRev acc, s)
    in
    _split (lazy Nil) n s

  let splitp f s = (prefix f s, suffix (fun a -> not (f a)) s)
end

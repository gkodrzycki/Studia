module SBTsig(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val flip : bool t
  val get : State.t t
  val put : State.t -> unit t
  val run : State.t -> 'a t -> 'a Seq.t
  end = struct  
    type 'a t = State.t -> ('a * State.t) Seq.t

    let return a s = Seq.return (a, s)

    let bind m f s =
      let x = m s in 
      Seq.flat_map (fun (a, s) -> f a s) x

    let fail s = Seq.empty
    let flip s = List.to_seq [(true, s); (false, s)]
    let get s = Seq.return(s, s)
    let put s _ = Seq.return ((), s)
    let run s m = fst (Seq.unzip (m s))
end
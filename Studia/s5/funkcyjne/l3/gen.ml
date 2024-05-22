module type Permutation = sig
  type key
  type t
  (** permutacja jako funkcja *)
  val apply : t -> key -> key
  (** permutacja identycznościowa *)
  val id : t
  (** permutacja odwrotna *)
  val invert : t -> t
  (** permutacja która tylko zamienia dwa elementy miejscami *)
  val swap : key -> key -> t
  (** złożenie permutacji (jako złożenie funkcji) *)
  val compose : t -> t -> t
  (** porównywanie permutacji *)
  val compare : t -> t -> int
end

module type S = 
    sig
        type t
        val is_generated : t -> t list -> bool
    end

module Make(Perm : Permutation) =
struct 
  module S = Set.Make(Perm)
  type t = Perm.t
  let is_generated (perm : t) (gen : t list) =
    let x0 = List.fold_left (fun s x -> S.add x s) S.empty gen in
    let rec next_step xn = 
      let inverse = S.fold (fun x s -> S.add (Perm.invert x) s) xn S.empty in
      let product = S.fold (fun a s -> S.union (S.fold (fun b s -> S.union (S.singleton (Perm.compose a b)) s) xn S.empty) s) xn S.empty 
    in let next = xn |> S.union inverse |> S.union product in 
    if S.equal next xn then false
    else if S.mem perm next then true
    else next_step next
  in next_step x0

end

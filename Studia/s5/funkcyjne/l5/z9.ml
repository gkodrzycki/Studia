type empty = |

type _ fin_type =
  | Empty : empty fin_type
  | Unit : unit fin_type
  | Bool : bool fin_type
  | Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
  | Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type

let rec all_values: type a. a fin_type -> a Seq.t = function
  | Empty -> Seq.empty
  | Unit -> Seq.return ()
  | Bool -> Seq.cons true (Seq.return false)
  | Pair (a, b) -> Seq.product (all_values a) (all_values b)
  | Either (a, b) ->
      Seq.append
        (Seq.map Either.left (all_values a))
        (Seq.map Either.right (all_values b))


List.of_seq (all_values (Either(Bool, Bool)));;
open Logic
type goal = Logic.formula * (string * Logic.formula) list

type proof_tree = 
| Hole of goal
| Theorem of Logic.theorem
| ImplI of proof_tree * goal
| ImplE of proof_tree * goal * proof_tree
| BotE of proof_tree * goal

type proof_tree_ctx = 
| Root
| Left of proof_tree_ctx * proof_tree * goal 
| Right of proof_tree_ctx * goal * proof_tree
| ChildB of proof_tree_ctx * goal 
| ChildI of proof_tree_ctx * goal 


type proof =
| Complete of Logic.theorem
| Incomplete of proof_tree_ctx * goal

let proof goal =
    Incomplete (Root, goal)

    
let goal pf =
  match pf with  
  | Complete (th) -> None
  | Incomplete (_, goal) -> Some goal
      
let qed pf =
  match pf with 
  | Complete (th) -> th
  | Incomplete (_, _) -> failwith "dziura ;(              "

(**************************************************************************)
let rec traverse_down ctx node =
  match node with
  | Hole goal -> Incomplete(ctx, goal)
  | Theorem _ -> failwith "dowiedzione wszystko :]"
  | ImplI(n1, goal) ->  traverse_down (ChildI(ctx, goal)) n1
  | ImplE(n1,goal,n2) -> traverse_down (Left(ctx, n2, goal)) n1
  | BotE(n1, goal) -> traverse_down (ChildB(ctx, goal)) n1

let rec traverse_up ctx node =
  match ctx with
  | Root -> traverse_down ctx node
  | Left(ctx, right, goal) -> traverse_down (Right(ctx, goal, node)) right
  | Right(ctx, goal, left) -> traverse_up ctx (ImplE(left, goal, node))
  | ChildI(ctx, goal) -> traverse_up ctx (ImplI(node, goal)) 
  | ChildB(ctx, goal) -> traverse_up ctx (BotE(node, goal)) 
  
let next pf =
  match pf with
  | Complete (th) -> failwith "nie ma kolejnego celu ;c"
  | Incomplete (ctx, goal) -> traverse_up ctx (Hole(goal))


(**************************************************************************)

let intro name pf =
  match pf with 
  | Incomplete (ctx, (formula, named_ass)) -> 
    begin match formula with
    | Implication(a, b) -> 
      let new_named = (name, a) :: named_ass in
      let new_ctx = ChildI(ctx, (formula, named_ass)) in
      let new_goal = (b, new_named) in
      Incomplete (new_ctx, new_goal)

    | _ -> failwith "to nie wygląda jak implikacja ;(" end
  | _ -> failwith "dowiedzione już :> "

(**************************************************************************)

let apply f pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()

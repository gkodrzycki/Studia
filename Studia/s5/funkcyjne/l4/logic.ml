type formula = 
| Neg
| Variable of string
| Implication of formula * formula

let string_of_formula f =
  let naw s =
    "(" ^ s ^ ")" in
  let rec rek f l =
    match f with
    | Variable s -> s
    | Neg -> "⊥"
    | Implication (a, b) -> let x = rek a true ^ " -> " ^ rek b false in 
      if l then naw x else x in
  rek f false

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem =
| Assumption of formula list * formula
| ImplI of theorem * formula list * formula
| ImplE of theorem * theorem * formula list * formula
| BotE of theorem * formula list * formula

let rec assumptions thm =
  match thm with 
  | Assumption (fl,f) -> []
  | ImplI (t,fl,f) -> List.append (assumptions t) fl
  | ImplE (t1,t2,fl,f) -> List.append fl (List.append (assumptions t1) (assumptions t2))
  | BotE (t,fl,f) -> List.append (assumptions t) fl




let rec consequence thm =
  match thm with 
  | Assumption (fl,f) -> f
  | ImplI (t,fl,f) -> f
  | ImplE (t1,t2,fl,f) -> f
  | BotE (t,fl,f) -> f


let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
  Assumption([f],f)

let imp_i f thm =
  ImplI(thm, assumptions(thm), Implication(f,consequence(thm)))

let imp_e th1 th2 =
  let x = consequence(th1) and y = consequence(th2) in match x,y with
| Implication(a,b),_ ->  ImplE(th1,th2,List.append (assumptions th1) (assumptions th2),b)
| _,_ -> failwith ";C"

let bot_e f thm =
  BotE(thm,assumptions(thm),f)

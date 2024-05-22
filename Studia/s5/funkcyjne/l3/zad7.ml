open Logic

let a = by_assumption (Variable "p")
let b = imp_i (Variable "p") a

let _ = (pp_print_theorem Format.std_formatter b)


let a = by_assumption (Variable "p")
let b = imp_i (Variable "q") a
let c = imp_i (Variable "p") b

let _ = (pp_print_theorem Format.std_formatter c)

let a = by_assumption Neg        
let b = bot_e (Variable "p") a   
let c = imp_i Neg b

let _ = (pp_print_theorem Format.std_formatter c)

let pqrL = (Implication (Variable "p", Implication (Variable "q", Variable "r")))
let pqrT = by_assumption pqrL
let pT = by_assumption (Variable "p")
let qrT = imp_e pqrT pT
let pqT = by_assumption (Implication ((Variable "p"), (Variable "q")))
let qT = imp_e pqT pT
let rT = imp_e qrT qT
let prT = imp_i (Variable "p") rT 
let pqprT  = imp_i (Implication (Variable "p", Variable "q")) prT
let pqrpqprT = imp_i pqrL pqprT

let _ = (pp_print_theorem Format.std_formatter pqrpqprT)

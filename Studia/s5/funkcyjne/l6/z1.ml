type ('a, 'b) format = (string -> 'b) -> (string -> 'a)

let lit tekst = 
  fun kontynuacja s -> kontynuacja(s^tekst)

let int kontynuacja = 
  fun s x -> kontynuacja(s^(string_of_int x))
  
let str kontynuacja = 
  fun s tekst -> kontynuacja(s^tekst)

let (^^) f g h = f(g h)

let sprintf fmt = 
  fmt (fun x -> x) ""
  
(* Test *)
sprintf (lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit ".") 5 "ów"
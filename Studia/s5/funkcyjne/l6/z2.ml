type ('a, 'b) format = 
| Int : (int -> 'a, 'a) format  
| Str : (string -> 'a, 'a) format
| Lit : string -> ('a, 'a) format
| Cat : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format  

let lit tekst = 
  fun kontynuacja s -> kontynuacja(s^tekst)

let inte kontynuacja = 
  fun s x -> kontynuacja(s^(string_of_int x))
  
let str kontynuacja = 
  fun s tekst -> kontynuacja(s^tekst)

let (^^) t1 t2 = Cat (t1, t2)

let rec helper : type a b. (a,b) format -> (string -> b) -> string -> a = function
  | Int -> (inte) 
  | Str -> (str)
  | Lit a -> lit a 
  | Cat (a, b) -> fun cont s -> helper a (helper b cont) s


let rec helper2 : type a b. (a,b) format -> (unit -> b) -> (unit -> a) = function
| Int -> fun kontynuacja () x -> print_int x; kontynuacja ()
| Str -> fun kontynuacja () x -> print_string x; kontynuacja ()
| Lit a -> fun kontynuacja () -> print_string a; kontynuacja ()
| Cat (a, b) -> fun kontynuacja () -> helper2 a (helper2 b kontynuacja) ()

let ksprintf exp kontynuacja = 
  helper exp kontynuacja ""

let sprintf exp = 
  ksprintf exp (fun x -> x)

let kprintf exp kontynuacja =
  helper2 exp (fun () -> kontynuacja) ()
  
let printf exp = 
  kprintf exp ()

(* Test *)

let test = (sprintf (Lit "Ala ma " ^^ Str)) "kota"
let test2 = printf (Lit "Ala ma " ^^ Str) "kota"

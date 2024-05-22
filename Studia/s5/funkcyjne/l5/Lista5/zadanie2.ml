let rec oldfix f x = f (oldfix f) x;;

let mutfix (func : (('a -> 'b) -> 'a -> 'b)) (elem : 'a) =
  let placeholder (func : (('a -> 'b) -> 'a -> 'b)) (elem : 'a) : 'b = 
    failwith "This do nothing" in let fix = ref placeholder in
    fix := (fun (func : (('a -> 'b) -> 'a -> 'b)) (elem : 'a) -> func (!fix func) elem);
    func (!fix func) elem;;

type 'a lambda_fix = Fix of ('a lambda_fix -> 'a);;
let unfix = function | Fix f -> f;;
let fixop f = (fun x a -> f (unfix x x) a)(Fix (fun x a -> f (unfix x x)a));;

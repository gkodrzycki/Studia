let rec fix f x = f (fix f) x;;
let rec fix_with_limit deepth func elem = 
  if deepth < 0 then failwith "Stack overflow"
  else func (fix_with_limit (deepth - 1) func) elem;;

open Hashtbl;;
let h = Hashtbl.create 100;;
let rec fix_memo f x = 
  try 
    Hashtbl.find h x
  with
  | Not_found -> let res = f (fix_memo f) x in 
    Hashtbl.add h x res ; Hashtbl.find h x;;

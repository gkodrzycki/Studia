module IdMonad = struct
  type 'a t

  let return a = a
  let bind a f = f a
end

module IdMonadL = struct
  type 'a t = unit -> 'a

  let return a () = a 
  let bind a f () = f (a ()) ()
end


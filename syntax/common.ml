
type uniform = 
   | Terminal of string 
   | Literal  of string 
   | Synchro  of string

let string_of_uniform = function
   | Literal  s -> s
   | Terminal s -> s
   | Synchro  s -> s

let uni_compare x y =
   String.compare (string_of_uniform x) (string_of_uniform y)

(* UniformSet *)
module UniformSet = Set.Make (
   struct
      type t = uniform
      let compare = uni_compare  
   end
)

let one_elem x = UniformSet.add x UniformSet.empty

let ( |>  ) l f = List.iter f l
let ( >>  ) l f = Hashtbl.iter f l
let ( >|> ) l f = UniformSet.iter f l

let ( $ ) f v = f v

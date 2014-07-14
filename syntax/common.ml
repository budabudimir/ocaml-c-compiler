
type uniform = 
  | UTerminal of string 
  | ULiteral  of string 
  | USynchro  of string

type action = 
  | TShift of int
  | TReduce of uniform * int
  | TError

let string_of_uniform = function
  | ULiteral  s -> s
  | UTerminal s -> s
  | USynchro  s -> s

let uniform_compare x y =
  String.compare (string_of_uniform x) (string_of_uniform y)

module UniformSet = Set.Make (
   struct
      type t = uniform
      let compare = uniform_compare  
   end
)

let one_elem x = UniformSet.add x UniformSet.empty

let ( >|> ) l f = UniformSet.iter f l

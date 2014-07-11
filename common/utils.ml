
let read_lines chn =
   let rec loop lines =
      try loop ((input_line chn) :: lines)
      with _ -> lines
   in List.rev (loop [])

let maximum = List.fold_left max min_int
let minimum = List.fold_left min max_int 

let rec unique xs ys =
   match xs, ys with
   | [], ys -> ys
   | xs, [] -> xs
   | x::xs, y::ys when x < y -> x :: unique xs (y::ys)
   | x::xs, y::ys when y < x -> y :: unique (x::xs) ys
   | x::xs, y::ys -> x :: unique xs ys

let rec drop n = function
   | h::t when n > 0 -> drop (n-1) t
   | rest -> rest

let rec take n = function
   | h::t when n > 0 -> h :: take (n-1) t
   | rest -> []

let rec exists_flatten f = function
   | h::t -> List.exists f h || exists_flatten f t
   | []   -> false

(* string utils *)
let s_from s i = String.sub s i (String.length s - i)


(* StringSet *)
module StringSet = Set.Make (String)

let ( >| ) l f = StringSet.iter f l

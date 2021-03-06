
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
let ( |> ) l f = List.iter f l
let ( >> ) l f = Hashtbl.iter f l
let ( $ ) f v = f v

let enumerate tbl f =
  let module H = Hashtbl in
  fun p ->
    if H.mem tbl p
    then H.find tbl p
    else begin 
      let r = 1 + H.length tbl in 
      H.add tbl p (1 + H.length tbl);
      f p;
      r
    end

let memoize tbl f =
  let module H = Hashtbl in
  fun p -> 
    if H.mem tbl p
    then H.find tbl p
    else (let v = f p in H.add tbl p v; v)

let mem_rec tbl f =
  let rec fn = lazy (memoize tbl (fun x -> f (Lazy.force fn) x)) in
  Lazy.force fn

let enum_rec tbl f =
  let rec fn = lazy (enumerate tbl (fun x -> f (Lazy.force fn) x)) in
  Lazy.force fn

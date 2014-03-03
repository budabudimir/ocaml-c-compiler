
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



let read_lines chn =
   let rec loop lines =
      try loop ((input_line chn) :: lines)
      with _ -> lines
   in List.rev (loop [])

type regexp =
   | Eps
   | Chr of char
   | Any of regexp * regexp
   | Fol of regexp * regexp
   | Rep of regexp

let regexp_of_string str = Eps

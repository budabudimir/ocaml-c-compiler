
let read_lines chn =
   let rec loop lines =
      try loop ((input_line chn) :: lines)
      with _ -> lines
   in List.rev (loop [])


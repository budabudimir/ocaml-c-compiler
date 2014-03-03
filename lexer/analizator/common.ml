type closure =
  | Eps
  | Char   of char
  | Follow of closure * closure
  | AnyOf  of closure * closure
  | Repeat of closure

type action =
  | NewLine
  | GoBack  of int
  | GoState of string
  | Lexeme  of string

type gen_rule = {
  gen_st   : string;
  gen_reg  : closure;
  gen_acts : action list;
}

let fail () = failwith "failure - bad format?"

let minimum = List.fold_left min max_int
let maximum = List.fold_left max min_int

let decons = function
  | hd :: tl -> hd, tl
  | _ -> fail ()

let read_lines chan =
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines


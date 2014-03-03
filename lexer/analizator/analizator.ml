open Printf
open Common

type rule = {
  st   : string;
  reg  : string -> int -> int list;
  acts : action list;
}

let eps str i = [i]

let char c str i =
  if i < String.length str && str.[i] = c then [i+1] else []

let rec unique xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x::xs, y::ys when x < y -> x :: unique xs (y::ys)
  | x::xs, y::ys when x > y -> y :: unique (x::xs) ys
  | x::xs, y::ys -> x :: unique xs ys

let follow f g str i =
  let end_f = f str i in
  List.fold_left unique [] (List.map (g str) end_f)

let any_of f g str i =
  unique (f str i) (g str i)

let repeat f str i =
  let rec loop = function
    | []  -> []
    | pos ->
      let m = minimum pos in
      let rest = List.filter (fun x -> x <> m) pos in
      m :: loop (unique (f str m) rest)
  in
  loop [i]

let convert_rule r =
  let rec constr = function
    | Eps          -> eps
    | Char c       -> char c
    | Follow (a,b) -> follow (constr a) (constr b)
    | AnyOf (a,b)  -> any_of (constr a) (constr b)
    | Repeat p     -> repeat (constr p)
  in
  { st   = r.gen_st;
    reg  = constr r.gen_reg;
    acts = r.gen_acts;
  }

let parse f str i =
  match f str i with
  | [] -> -1
  | ends -> maximum ends

let run_lexer init rules =
  let state  = ref init in
  let line   = ref 1 in
  let source = String.concat "\n" (read_lines stdin) in
  let rec loop pos =
    if pos = String.length source then ()
    else
      let rules   = List.filter (fun r -> r.st = !state) rules in
      let results = List.map (fun r -> parse r.reg source pos, r) rules in
      let ending  = maximum (List.map fst results) in
      let _, rule = List.find (fun (p,_) -> p = ending) results in
      if ending <= pos then begin
        eprintf "%d: error: unexpected char `%c`\n" !line source.[pos];
        loop (pos + 1)
      end else begin
        let next = ref ending in
        let lex  = ref "" in
        List.iter (function
          | NewLine   -> incr line
          | GoBack  b -> next := pos + b
          | GoState s -> state := s
          | Lexeme  l -> lex := l
        ) rule.acts;
        if !lex <> "" then
          printf "%s %d %s\n" !lex !line (String.sub source pos (!next - pos));
        loop !next
      end
  in
  loop 0

let _ =
  let (init : string), (rules : gen_rule list) = Marshal.from_string Lang.data 0 in
  let rules = List.map convert_rule rules in
  run_lexer init rules


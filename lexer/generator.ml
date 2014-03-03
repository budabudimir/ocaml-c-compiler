open Printf
open Scanf
open Common

let rec unfold_string defs str =
  let process_item = function
    | Str.Text  s -> s
    | Str.Delim s ->
      let s = (String.sub s 1 (String.length s - 2)) in
      let s = Hashtbl.find defs s in
      "(" ^ unfold_string defs s ^ ")"
  in
  let items = Str.full_split (Str.regexp "{[a-zA-Z]*}") str in
  String.concat "" (List.map process_item items)

let make_defs lines =
  let parse line = sscanf line "{%[a-zA-Z]} %s" (fun a b -> a,b) in
  let items = List.map parse lines in
  let h = Hashtbl.create (List.length items) in
  List.iter (fun (k,v) -> Hashtbl.add h k v) items;
  h

let make_init line =
  let items = Str.split (Str.regexp " ") line in
  List.nth items 1

let parse_actions lines =
  let rec go = function
    | "}" :: rest -> []
    | str :: rest ->
      let n = String.length str in
      let act = match str.[0] with
        | 'U' -> GoState (String.sub str 14 (n-14))
        | 'N' -> NewLine
        | 'V' -> GoBack (int_of_string (String.sub str 9 (n-9)))
        | _ -> fail ()
      in
      act :: go rest
    | _ -> fail ()
  in
  match lines with
  | "{" :: "-" :: rest -> go rest
  | "{" :: lex :: rest -> Lexeme lex :: go rest
  | _ -> fail ()

let make_rules lines defs =
  let rec split = function
    | [] -> []
    | "}" :: rest -> ["}"] :: split rest
    | hd :: tl -> let same, rest = decons (split tl) in (hd :: same) :: rest
  in
  List.map (function
  | [] -> fail ()
  | hd :: tl ->
    let st, reg = sscanf hd "<%[_a-zA-z]>%s" (fun a b -> a,b) in
    let acts    = parse_actions tl in
    let reg     = unfold_string defs reg in
    { gen_st = st;
      gen_reg = Parser.create reg;
      gen_acts = acts;
    }
  ) (split lines)

let write_lang init rules =
  let str = Marshal.to_string (init, rules) [] in
  let out = open_out "analizator/lang.ml" in
  fprintf out "let data = \"";
  String.iter (fun c -> fprintf out "\\%03d" (int_of_char c)) str;
  fprintf out "\"\n"

let main () =
  let input = read_lines stdin in
  let take_defs = List.partition (fun x -> x.[0]='{' && x<>"{") in
  match take_defs input with
  | defs, states :: lexemes :: rules ->
    let init  = make_init states in
    let defs  = make_defs defs in
    let rules = make_rules rules defs in
    write_lang init rules
  | _ ->
    fail ()

let _ = if !Sys.interactive then () else main ()

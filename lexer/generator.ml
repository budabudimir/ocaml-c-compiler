
open Printf
open Scanf
open Utils
open Lexer
open Tools

let make_regexes defs =
   let parse line = sscanf line "{%[a-zA-Z]} %s" (fun x y -> x, y) in
   let hsh = Hashtbl.create (List.length defs) in
   let insert l = let k, v = parse l in Hashtbl.add hsh k v in
   List.iter insert defs; 
   hsh

let rec genstr s tbl =
   let rec process_item = function
      | Str.Text s -> s
      | Str.Delim s -> 
            let r = String.sub s 1 (String.length s - 2) in
            genstr (Hashtbl.find tbl r) tbl
   in
   let items = Str.full_split (Str.regexp "{[a-zA-Z]*}") s in
   String.concat "" (List.map process_item items)

let action_of_string a =
   let sec x = List.nth (Str.split (Str.regexp " ") a) 1 in
   match a.[0] with 
   | 'U' -> GoState (sec a)
   | 'V' -> GoBack  (int_of_string (sec a))
   | 'N' -> NewLine
   |  x  -> failwith (sprintf "Lexer: Unexpected first letter in action parsing %c" x)

let rec get_actions = function
   | "}" :: _ -> []
   | a   :: r -> action_of_string a :: get_actions r
   | _        -> failwith "Lexer: parsing actions error"

let rec make_rules tbl = function 
   | name :: "{" :: lex :: r -> 
         let new_gen_rule regx acts stat = { 
            regx = regexp_of_string (genstr regx tbl); 
            acts = acts; 
            stat = stat;
         } in

         let acts = if lex = "-" then get_actions r 
                    else (Lexeme lex) :: get_actions r in
         let stat, regx = sscanf name "<%[_a-zA-Z]>%s" (fun x y -> (x,y)) in
         new_gen_rule regx acts stat :: make_rules tbl r
   | h :: r -> make_rules tbl r
   | [] -> []

let _ =
   let lines = read_lines stdin in
   let reg_defs = List.partition (fun x -> x.[0] = '{' && x <> "{") in
   match reg_defs lines with
   | defs, states :: lexemes :: rules ->
         let tbl = make_regexes defs in
         let lan_rules = make_rules tbl rules in
         let init_state = List.nth (Str.split (Str.regexp " ") states) 1 in

         let str = Marshal.to_string (init_state, lan_rules) [] in
         let out = open_out "lang.ml" in
         fprintf out "let data = \"";
         String.iter (fun x -> fprintf out "\\%03d" (int_of_char x)) str;
         fprintf out "\"\n"
   | _ -> failwith "Lexer: Reading from description file failed" 


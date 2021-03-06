
open Printf
open Utils
open Common
open Automaton

module SS = StringSet
module H  = Hashtbl
module U  = UniformSet
module L  = List
module I  = ItemSet
module IS = ItemSetSet

let emptyTerm = UTerminal "$"
let startLitr = ULiteral  "q0"
let all_unis_n = 100

let productions = H.create all_unis_n
let terminals = ref SS.empty
let synchrons = ref SS.empty
let literals  = ref SS.empty

let getProds = function 
  | (ULiteral s) as l -> H.find productions l
  | _ -> []

let uniform_of_string s =
   if SS.mem s !terminals      then UTerminal s
   else if SS.mem s !literals  then ULiteral  s
   else if SS.mem s !synchrons then USynchro  s
   else failwith $ "Syntax: string not recognized as uniform character (" ^ s ^ ")"

let rec create_rules lines = 
   let rec get_prod = function
      | h::t when h.[0] = ' ' -> 
        let prod = Str.split (Str.regexp " ") (s_from h 1) in
        let prod = L.map uniform_of_string prod in
        prod :: get_prod t
      | _ -> []
   in
   match lines with
   | h::t when h.[0] = '<' -> (uniform_of_string h, get_prod t) :: create_rules t
   | h::t -> create_rules t
   | [] -> []

let is_nullable = function
  | (ULiteral s) as l -> exists_flatten ((=) emptyTerm) $ getProds l
  | UTerminal "$"     -> true
  | _                 -> false

let first = H.create all_unis_n

let find_first = mem_rec first (fun find_first elem -> 
  H.add first elem U.empty;
  match elem with
  | (ULiteral s) as l -> 
    let prods = getProds l in
    let rec mapf = function
      | (ULiteral h as x) :: t -> 
        if is_nullable x 
        then U.union (find_first x) (mapf t)
        else find_first x
      | (UTerminal h as x) :: t -> one_elem x
      | (USynchro  h as x) :: t -> one_elem x
      | [] -> U.empty
    in
    let firsts = L.map mapf prods in
    L.fold_left U.union U.empty firsts;
  | UTerminal s as l -> one_elem l
  | USynchro  s as l -> one_elem l
)

let rec prod_first l = 
  match l with
  | h::t when is_nullable h -> U.union (find_first h) $ prod_first t
  | (ULiteral s as h) :: t  -> find_first h
  | h :: t                  -> one_elem h
  | []                      -> U.empty

  
let maps_size = 10007
let closures = H.create maps_size
let goto_map = H.create maps_size

let rec closure = mem_rec closures (fun closure old_set ->
  let new_set = I.fold (fun x result ->
    match x with 
    | _,_,h::t,look ->
      let prods = getProds h in
      let terms = prod_first $ t @ [look] in
      let sets = L.fold_left (fun result prod ->
        let set = U.fold (fun b r -> I.add (0,h,prod,b) r) terms I.empty in
        I.union result set
      ) I.empty prods in
      I.union result sets
    | _ -> result
  ) old_set I.empty in 
  let new_set = I.union new_set old_set in
  if I.subset new_set old_set 
  then new_set
  else closure new_set
)

let goto = mem_rec goto_map (fun goto (i,x) ->
  closure (I.fold (fun item result ->
    match item with 
    | len,left,h::t,look when h = x -> 
        I.add (len+1,left,t,look) result
    | _ -> result
  ) i I.empty)
)

let rec gen_items old_set symbols =
  let new_sets = IS.fold (fun i c -> 
    let goto_sets = L.fold_left (fun r x -> 
      IS.add (goto (i,x)) r) IS.empty symbols in
    IS.union c goto_sets) old_set IS.empty in
  let new_set = IS.union old_set new_sets in
  if IS.subset new_set old_set 
  then old_set
  else gen_items new_set symbols

let reduce i a =
  let reduce = I.fold (fun x r ->
    match x with
    | (l,c,[],a') when a = a' -> (l,c)::r
    | _ -> r
  ) i [] in
  match reduce with
  | [x] -> Some x
  | []  -> None
  | _   -> failwith "Ambigous reduction"

let _ =
  let input = read_lines stdin in
  match input with 
  | vars :: terms :: syncs :: rules ->
    let litrs = drop 1 (Str.split (Str.regexp " ") vars) in
    let terms = drop 1 (Str.split (Str.regexp " ") terms) in
    let syncs = drop 1 (Str.split (Str.regexp " ") syncs) in

    literals  := SS.union !literals  $ L.fold_right SS.add litrs SS.empty;
    terminals := SS.union !terminals $ L.fold_right SS.add terms SS.empty;
    synchrons := SS.union !synchrons $ L.fold_right SS.add syncs SS.empty;

    let ulit = L.map uniform_of_string litrs in
    let utrm = L.map uniform_of_string terms in
    let usyn = L.map uniform_of_string syncs in

    let all = ulit @ utrm @ usyn in
    let rules = create_rules rules in

    rules |> (fun (left,rajt) -> H.add productions left rajt);

    let startingSet = IS.add (
      closure (I.add (0,startLitr,[List.hd ulit], emptyTerm) I.empty)) IS.empty in

    let c = gen_items startingSet all in
    let enum = H.create all_unis_n in
    let i = ref 0 in
    
    IS.iter (fun x -> H.add enum x !i; incr i) c;

    let action_table = H.create all_unis_n in

    IS.iter (fun i ->
      L.iter (fun a -> 
        match reduce i a with
        | Some (l,c) -> 
            H.add action_table (i,a) $ TReduce (c,l)
        | None when H.mem goto_map (i,a)-> 
            H.add action_table (i,a) $ TShift (H.find enum (goto (i,a)))
        | _ -> 
            H.add action_table (i,a) TError
      ) all
    ) c;

    let str = Marshal.to_string action_table [] in
    let out = open_out "lang.ml" in
    fprintf out "let data = \"";
    String.iter (fun c -> fprintf out "\\%03d" (int_of_char c)) str;
    fprintf out "\"\n";
  | _ -> failwith "Syntax: wrong data descriptor"

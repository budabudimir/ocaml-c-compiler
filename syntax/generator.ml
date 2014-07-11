
open Printf
open Scanf
open Utils
open Common

let emptyTerm = Terminal "$"

let all_unis_n = 50

let first       = Hashtbl.create all_unis_n
let productions = Hashtbl.create all_unis_n

let terminals = ref (StringSet.empty)
let synchrons = ref (StringSet.empty)
let literals  = ref (StringSet.empty)

let getProds = function 
   | (Literal s) as l -> Hashtbl.find productions l
   | _ -> []

let uniform_of_string s =
   let module S = StringSet in
   if S.mem s !terminals      then Terminal s
   else if S.mem s !literals  then Literal  s
   else if S.mem s !synchrons then Synchro  s
   else failwith $ "Syntax: string not recognized as uniform character (" ^ s ^ ")"

let rec create_rules lines = 
   let rec get_prod = function
      | h::t when h.[0] = ' ' -> 
        let prod = Str.split (Str.regexp " ") (s_from h 1) in
        let prod = List.map uniform_of_string prod in
        prod :: get_prod t
      | _ -> []
   in
   match lines with
   | h::t when h.[0] = '<' -> (uniform_of_string h, get_prod t) :: create_rules t
   | h::t -> create_rules t
   | [] -> []

let is_nullable = function
   | (Literal s) as l -> exists_flatten ((=) emptyTerm) $ getProds l
   | Terminal "$"     -> true
   | _                -> false

let rec find_first elem =
    let module H = Hashtbl in
    if H.mem first elem
    then H.find first elem
    else begin
      H.add first elem UniformSet.empty;
      let f = find_first' elem in
      H.add first elem f;
      f
    end;
and find_first' = function
   | (Literal s) as l -> 
     let prods = getProds l in
     let module S = UniformSet in
     let rec mapf = function
        | ((Literal h) as x) :: t -> 
          if is_nullable x 
          then S.union (find_first x) (mapf t)
          else find_first x
        | ((Terminal h) as x) :: t -> one_elem x
        | ((Synchro  h) as x) :: t -> one_elem x
        | [] -> S.empty
     in
     let firsts = List.map mapf prods in
     List.fold_left S.union S.empty firsts;
   | Terminal s as l -> one_elem l
   | Synchro  s as l -> one_elem l

let _ =
   let input = read_lines stdin in
   match input with 
   | vars :: terms :: syncs :: rules ->
     let litrs = drop 1 (Str.split (Str.regexp " ") vars) in
     let terms = drop 1 (Str.split (Str.regexp " ") terms) in
     let syncs = drop 1 (Str.split (Str.regexp " ") syncs) in

     let module SS = StringSet in
     literals  := SS.union !literals  $ List.fold_right SS.add litrs SS.empty;
     terminals := SS.union !terminals $ List.fold_right SS.add terms SS.empty;
     synchrons := SS.union !synchrons $ List.fold_right SS.add syncs SS.empty;

     let rules = create_rules rules in
     rules |> (fun (left,rajt) -> Hashtbl.add productions left rajt);
     let all = List.map uniform_of_string $ litrs @ terms @ syncs in
     all |> (fun x -> Hashtbl.add first x $ find_first x);
   | _ -> failwith "Syntax: wrong data descriptor"


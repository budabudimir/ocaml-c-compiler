
open Printf
open Scanf
open Utils
open Common

let emptyTerm = Terminal "$"

let all_unis_n = 50

let first       = Hashtbl.create all_unis_n
let productions = Hashtbl.create all_unis_n

let terminals = ref StringSet.empty 
let synchrons = ref StringSet.empty
let literals  = ref StringSet.empty

let getProds = function 
   | (Literal s) as l -> Hashtbl.find productions l
   | _         -> []

let uniform_of_string s =
   if StringSet.mem s !terminals      then Terminal s
   else if StringSet.mem s !literals  then Literal  s
   else if StringSet.mem s !synchrons then Synchro  s
   else failwith "Syntax: string not recognized as uniform character"

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
   | (Literal s) as l -> exists_flatten ((=) emptyTerm) (getProds l)
   | Terminal "$"     -> true
   | _                -> false


let rec find_first = function
   | (Literal s) as l -> 
         let prods = getProds l in

         let rec mapf = function
            | (Literal h) as x :: t -> 
                  if is_nullable x then UniformSet.union (find_first x) (mapf t)
                  else find_first x
            | (Terminal h) as x :: t -> one_elem x
            | (Synchro  h) as x :: t -> one_elem x
            | [] -> UniformSet.empty
         in

         let firsts = List.map mapf prods in
         List.fold_left UniformSet.union UniformSet.empty firsts;
   | Terminal s as l -> one_elem l
   | Synchro  s as l -> one_elem l

 
let _ =
   let input = read_lines stdin in
   match input with 
   | vars :: terms :: syncs :: rules ->
         let litrs = drop 1 (Str.split (Str.regexp " ") vars) in
         let terms = drop 1 (Str.split (Str.regexp " ") terms) in
         let syncs = drop 1 (Str.split (Str.regexp " ") syncs) in

         let rules = create_rules rules in
         List.iter (fun (left, rajt) -> Hashtbl.add productions left rajt) rules;

         literals  := List.fold_right (StringSet.add) litrs StringSet.empty;
         terminals := List.fold_right (StringSet.add) terms StringSet.empty;
         synchrons := List.fold_right (StringSet.add) syncs StringSet.empty;

         Hashtbl.iter (fun x y -> 
            printf "%s:\n" (string_of_uniform x);
            List.iter (
               List.iter (fun a -> printf "%s, " (string_of_uniform a))
                  ) y) productions;

         let all = List.map uniform_of_string (litrs @ terms @ syncs) in
         List.iter (fun x -> Hashtbl.add first x (find_first x)) all;
   | _ -> failwith "Syntax: wrong data descriptor"


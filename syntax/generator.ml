
open Printf
open Scanf
open Utils
open Common
open Automaton

module SS = StringSet
module H  = Hashtbl
module U  = UniformSet
module L  = List

let emptyTerm = UTerminal "$"

let all_unis_n = 50
let first       = H.create all_unis_n
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
  | h::t when is_nullable h -> U.union (H.find first h) $ prod_first t
  | (ULiteral s as h) :: t  -> H.find first h
  | h :: t                  -> one_elem h
  | []                      -> U.empty

let closures = H.create 50

let closure_one = mem_rec closures (fun closure_one -> function
  | _, h::t, a ->
    let productions' = getProds h in
    let terminals' = prod_first $ t @ [a] in
    let new_set = L.fold_left (@) [] $ L.map (fun x ->
      U.fold (fun y a -> ([],x,y) :: a) terminals' []) productions' in
    new_set @ (L.fold_left (@) [] $ L.map closure_one new_set)
  | _, [], a -> []
)

let closure_set i =
  i @ (L.fold_left (@) [] $ L.map closure_one i)

let goto i x =
  let new_set = L.fold_left (fun result -> function
    | r, h::t, a when h = x -> (h::r,t,a) :: result
    | _ -> result
  ) [] i in
  closure_set new_set

let rec gen_automaton items symbols =
  let transitions = L.map (fun x ->
    let next_rl_items = goto items x in
    let next_state = gen_automaton next_rl_items symbols in 
    (x, next_state)
  ) symbols in
  new_state items transitions

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

    let rules = create_rules rules in
    rules |> (fun (left,rajt) -> H.add productions left rajt);
    let all = L.map uniform_of_string $ litrs @ terms @ syncs in
    all |> (fun x -> H.add first x $ find_first x);
  | _ -> failwith "Syntax: wrong data descriptor"

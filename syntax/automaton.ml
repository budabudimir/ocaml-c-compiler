
open Printf
open Common
open Utils

(* left_production_part, right_production_part, lookahead *)
(* length, left_side, right_side, lookahead *)
type rl_item = int * uniform * uniform list * uniform

type state = {
  transitions : (uniform * int) list;
  rl_items    : rl_item                  list;
}

let new_state i t = {
  transitions = t;
  rl_items    = i;
}

let output_production prod =
  String.concat "" $ List.map string_of_uniform prod

let output_item (length, left_side, right_side, lookahead) =
  printf "(item|%d,%s,%s,%s)\n" 
    length 
    (string_of_uniform left_side) 
    (output_production right_side)
    (string_of_uniform lookahead)

let rec prod_compare x y =
  match x,y with
  | [], [] -> 0
  | [], _  -> -1
  | _, []  -> 1
  | xa::ya, xb::yb when uniform_compare xa xb = 0 -> prod_compare ya yb
  | xa::_, xb::_ -> uniform_compare xa xb

let compare_rl_item (len_a,lft_a,rgt_a,lok_a) (len_b,lft_b,rgt_b,lok_b) =
  let c = compare len_a len_b in
  if c <> 0 
  then c
  else (
    let c = uniform_compare lft_a lft_b in
    if c <> 0
    then c
    else (
      let c = uniform_compare lok_a lok_b in
      if c <> 0 
      then c
      else prod_compare rgt_a rgt_b
    )
  )

module ItemSet = Set.Make (
  struct
    type t = rl_item
    let compare = compare_rl_item
  end
)

module ItemSetSet = Set.Make (
  struct 
    type t = ItemSet.t
    let compare = ItemSet.compare
  end
)

let output_set i =
  ItemSet.iter output_item i

let output_sets c =
  ItemSetSet.iter (fun x -> 
    output_set x;
    printf "\n";
  ) c

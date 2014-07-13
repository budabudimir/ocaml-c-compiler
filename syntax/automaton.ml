
open Common

(* left_production_part, right_production_part, lookahead *)
type rl_item = uniform list * uniform list * uniform list

type state = {
  transitions : (uniform * state) list;
  rl_items    : rl_item           list;
}

let new_state t i = {
  transitions = t;
  rl_items    = i;
}

let state_compare a b = -1

module StateSet = Set.Make (
  struct
    type t = state
    let compare = state_compare
  end
)

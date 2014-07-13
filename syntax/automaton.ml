
open Common

(* left_production_part, right_production_part, lookahead *)
type rl_item = uniform list * uniform list * uniform

type state = {
  transitions : (uniform * state) list;
  rl_items    : rl_item           list;
}

let new_state i t = {
  transitions = t;
  rl_items    = i;
}

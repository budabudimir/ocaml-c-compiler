
open Printf
open Scanf
open Utils
open Tools

let _ =
   let (rules : gen_rule list), (state : string) = 
      Marshal.from_string Lang.data 0 in
   ()

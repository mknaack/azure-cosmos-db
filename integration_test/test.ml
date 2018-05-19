open Lwt
open Src
open Databases
(* list_databases *)

let p = list_databases endpoint
let px = p >>= content
let result = Lwt_main.run px
let _ = print_string result




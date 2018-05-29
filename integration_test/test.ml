open Lwt
open Src
open Databases

(* list_databases *)

let p = list_databases endpoint
let px = p >>= content
let result = Lwt_main.run px
let dbs = convert_list_databases result
(* let dbs_names = List.map (fun x -> x) dbs.databases *)
(* let xxx = List.map (fun x -> x.) dbs_names *)
let _ = print_string result
(* let dbs_names =  List.map (fun x -> x.id) dbs.databases *)






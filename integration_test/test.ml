open Lwt
open Src
open Databases

(* list_databases *)

module MyAuthKeys : Auth_key = struct
  let master_key = "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg=="
  let endpoint = "mknnack"
end

module MyAuth = Auth(MyAuthKeys)

module D = Database(MyAuth)

let p = D.list_databases ()
let stat = p >>= status
let px = p >>= content
let result = Lwt_main.run px
let status_result = Lwt_main.run stat
(* let dbs = D.convert_list_databases result *)
(* let dbs_names = List.map (fun x -> x) dbs.databases *)
(* let xxx = List.map (fun x -> x.) dbs_names *)
let _ = print_endline status_result
let _ = print_endline result
(* let dbs_names =  List.map (fun x -> x.id) dbs.databases *)


(* create database *)

let p = D.create "test"
let stat = p >>= status
let px = p >>= content

let result = Lwt_main.run px
let status_result = Lwt_main.run stat
let _ = print_endline status_result
let _ = print_string result





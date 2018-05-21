open Lwt
open Src
open Databases

(* list_databases *)

let p = list_databases endpoint
let px = p >>= content
let result = Lwt_main.run px
let _ = print_string result

(*
{"_rid":"","Databases":[{"id":"test","_rid":"1zxpAA==","_self":"dbs\/1zxpAA==\/","_etag":"\"00007e01-0000-0000-0000-5b0042840000\"","_colls":"colls\/","_users":"users\/","_ts":1526743684}],"_count":1}
*)





open Lwt
open Yojson


(*
- create database
- list database
*)

let authorization_token_using_master_key = Utility.authorization_token_using_master_key

  
let authorization date = (* "type=master&ver=1.0&sig=" ^ key *)
  let master_key = "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg==" in
  let verb = "GET" in (* get, post, put *)
  let resource_type = "dbs" in (* "dbs", "colls", "docs". *)
  (* let resource_id = "dbs/mknnack" in *)
  let resource_id = "" in
  let result = authorization_token_using_master_key verb resource_type resource_id date master_key in
  result
let endpoint = "mknnack"

  
  (* a simple function to access the content of the response *)
(* let content = function *)
(*   | { Ocsigen_http_frame.frame_content = Some v } -> *)
(*       let r = Ocsigen_stream.string_of_stream 100000 (Ocsigen_stream.get v) in *)
(*       let _ = Ocsigen_stream.finalize v in *)
(*       r *)
(*   | _ -> return "" *)

(* let json_data = `Assoc [("a", `Int 3)] *)

(* let json_content = *)
(*   `List [ *)
(*   `Assoc [ *)
(*   ("eventId", `String "fbf4a1a1-b4a3-4dfe-a01f-ec52c34e16f7"); *)
(*   ("eventType", `String "event-type"); *)
(*   (\* ("data", `Assoc [("a", `Int 5)]); *\) *)
(*   ("data", json_data); *)
(* ] *)
(* ] *)

(* let post_content = Yojson.pretty_to_string json_content *)

(* let _ = print_endline post_content *)
    
(* let content_type = "application", "json" *)
    
(* let p = Ocsigen_http_client.post_string ~host:"localhost" ~uri:"/streams/newstream" ~port:2113 ~content:post_content ~content_type:content_type () *)
(* let px = p >>= content   *)

(* let result = Lwt_main.run px *)
(* let _ = print_string result *)
let content = function
  | { Ocsigen_http_frame.frame_content = Some v } ->
      let r = Ocsigen_stream.string_of_stream 100000 (Ocsigen_stream.get v) in
      let _ = Ocsigen_stream.finalize v in
      r
  | _ -> return ""

(* let list_databases = *)
  

  
let create databaseaccount name =
  (* let post_content = *)
  (*   let json_content = *)
  (*     `Assoc [ *)
  (*     ("id", `String name); *)
  (*   ] *)
  (*   in *)
  (*   Yojson.pretty_to_string json_content *)
  (* in *)
  (* let content_type = "application", "json" in *)
  let ms_date =
    let now = Unix.time () in
    (* let zone = Netdate.localzone in *)
    (* Netdate.mk_mail_date ~zone now *)
    Utility.x_ms_date now
  in
  let _ = print_endline ms_date in
  let headers =
    Http_headers.empty
  |> Http_headers.add (Http_headers.name "authorization") (authorization ms_date)
  (* |> Http_headers.add (Http_headers.name "x-ms-version") "2017-02-22" *)
  |> Http_headers.add (Http_headers.name "x-ms-version") "2015-08-06"
  |> Http_headers.add (Http_headers.name "x-ms-date") ms_date
  (* |> Http_headers.add (Http_headers.name "content-type") ms_date *)
  in
  let post = Ocsigen_http_client.get
      ~https:true
      ~host: (databaseaccount ^ ".documents.azure.com")
      ~uri:"/dbs"
      ~headers
      ~port:443
      (* ~content:"" *)
      (* ~content:post_content *)
      (* ~content_type *)
      ()
  in
  post
    
(* let post_content = create "mknaack" "test" *)
(* let _ = print_endline post_content *)
                                 
(* let content_type = "application", "vnd.eventstore.events+json" *)
                                 
(* let p = Ocsigen_http_client.post_string ~host:"localhost" ~uri:"/streams/newstream" ~port:2113 ~content:post_content ~content_type:content_type () *)




let p = create endpoint "test"
let px = p >>= content

let result = Lwt_main.run px
let _ = print_string result

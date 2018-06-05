open Lwt
open Yojson


(*
- create database
- list database
*)

(* let authorization_token_using_master_key = Utility.authorization_token_using_master_key *)

module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module type Account = sig
  type verb = Get | Post | Put | Delete
  val authorization : verb -> string -> string
  val endpoint : string
end

module Auth (Keys : Auth_key) : Account = struct
  type verb = Get | Post | Put | Delete

  let string_of_verb = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"
  
  let authorization verb date = (* "type=master&ver=1.0&sig=" ^ key *)
    let verb = string_of_verb verb in (* get, post, put *)
    let resource_type = "dbs" in (* "dbs", "colls", "docs". *)
    (* let resource_id = "dbs/mknnack" in *)
    let resource_id = "" in
    let result = Utility.authorization_token_using_master_key verb resource_type resource_id date Keys.master_key in
    result

  let endpoint = Keys.endpoint
end

let content = function
  | { Ocsigen_http_frame.frame_content = Some v } ->
      let r = Ocsigen_stream.string_of_stream 100000 (Ocsigen_stream.get v) in
      let _ = Ocsigen_stream.finalize v in
      r
  | _ -> return ""

(* list databases: *)
let convert_list_databases s =
  Json_converter_j.list_databases_of_string s

module Database (Account : Account) = struct
  let headers verb =
    let ms_date =
      let now = Unix.time () in
      Utility.x_ms_date now
      in
      Http_headers.empty
      |> Http_headers.add (Http_headers.name "authorization") (Account.authorization verb ms_date)
      |> Http_headers.add (Http_headers.name "x-ms-version") "2017-02-22"
      |> Http_headers.add (Http_headers.name "x-ms-date") ms_date
  
  let list_databases () =
    let headers = headers Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host: (Account.endpoint ^ ".documents.azure.com")
        ~uri:"/dbs"
        ~headers
        ~port:443
        ()
    in
    get

  (* (\* create database: *\) *)

  let create name =
    let post_content =
      let value = ({id = name}: Json_converter_j.create_database) in
      Json_converter_j.string_of_create_database value
    in
    let content_type = "application", "json" in
    let headers = headers Account.Post in
    let post = Ocsigen_http_client.post_string
        ~https:true
        ~host: (Account.endpoint ^ ".documents.azure.com")
        ~uri:"/dbs"
        ~headers
        ~port:443
        ~content:post_content
        ~content_type
        ()
    in
    post
end

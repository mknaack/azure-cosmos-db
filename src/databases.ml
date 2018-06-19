open Lwt
open Yojson

module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module type Account = sig
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs
  val authorization : verb -> resource -> string -> string -> string
  val endpoint : string
end

module Auth (Keys : Auth_key) : Account = struct
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs

  let string_of_verb = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"

  let string_of_resource = function
    | Dbs -> "dbs"
    | Colls -> "colls"
    | Docs -> "docs"

  let authorization verb resource date db_name = (* "type=master&ver=1.0&sig=" ^ key *)
    let verb = string_of_verb verb in (* get, post, put *)
    let resource_type = string_of_resource resource in (* "dbs", "colls", "docs". *)
    let resource_id = db_name in
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
  let headers resource verb db_name =
    let ms_date =
      let now = Unix.time () in
      Utility.x_ms_date now
      in
      Http_headers.empty
      |> Http_headers.add (Http_headers.name "authorization") (Account.authorization verb resource  ms_date db_name)
      |> Http_headers.add (Http_headers.name "x-ms-version") "2017-02-22"
      |> Http_headers.add (Http_headers.name "x-ms-date") ms_date

  let host = Account.endpoint ^ ".documents.azure.com"

  let list_databases () =
    let headers = headers Account.Dbs Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host
        ~uri:"/dbs"
        ~headers: (headers "")
        ~port:443
        ()
    in
    get

  (* create database: *)

  let create name =
    let post_content =
      let value = ({id = name}: Json_converter_j.create_database) in
      Json_converter_j.string_of_create_database value
    in
    let content_type = "application", "json" in
    let headers = headers Account.Dbs Account.Post in
    let post = Ocsigen_http_client.post_string
        ~https:true
        ~host
        ~uri:"/dbs"
        ~headers: (headers "")
        ~port:443
        ~content:post_content
        ~content_type
        ()
    in
    post

  let get name =
    let headers = headers  Account.Dbs Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host
        ~uri: ("/dbs/" ^ name)
        ~headers: (headers ("dbs/" ^ name))
        ~port:443
        ()
    in
    get

  let delete ?v6 ?https ?port ?headers ~host ~uri () =
    Ocsigen_lib.Ip_address.get_inet_addr ?v6 host >>= fun inet_addr ->
    Ocsigen_http_client.raw_request
      ?https
      ?port
      ?headers
      ~http_method:Ocsigen_http_frame.Http_header.DELETE
      ~content:None
      ~host:(match port with None -> host | Some p -> host^":"^string_of_int p)
      ~inet_addr
      ~uri
      ()
      ()

  let delete name =
    let headers = headers Account.Dbs Account.Delete in
    let command = delete
        ~https:true
        ~host
        ~uri: ("/dbs/" ^ name)
        ~headers: (headers ("dbs/" ^ name))
        ~port:443
        ()
    in
    command

  module Collection = struct
    let list dbname =
      let headers = headers Account.Colls Account.Get in
      let get = Ocsigen_http_client.get
          ~https:true
          ~host
          ~uri:("/dbs/" ^ dbname ^ "/colls")
          ~headers: (headers ("dbs/" ^ dbname))
          ~port:443
        ()
      in
      get


    let create dbname coll_name =
      let post_content =
        let value = ({id = coll_name; indexingPolicy = None; partitionKey = None}: Json_converter_j.create_collection) in
        Json_converter_j.string_of_create_collection value
      in
      let content_type = "application", "json" in
      let headers = headers Account.Colls Account.Post in
      let post = Ocsigen_http_client.post_string
          ~https:true
          ~host
          ~uri:("/dbs/" ^ dbname ^ "/colls")
          ~headers: (headers ("dbs/" ^ dbname))
          ~port:443
          ~content:post_content
          ~content_type
          ()
      in
      post

  let get name coll_name =
    let headers = headers  Account.Colls Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host
        ~uri: ("/dbs/" ^ name ^ "/colls/" ^ coll_name)
        ~headers: (headers ("dbs/" ^ name^ "/colls/" ^ coll_name))
        ~port:443
        ()
    in
    get

  end
end
